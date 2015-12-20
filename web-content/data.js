/* ============================================================
   Data Types and Storage */

/* Types:

Config = {
  managers: {manager : [owner/repo, ...], ...},
  branch_day: {owner/repo: sha, ...},
}

ServerRepoInfo = {
  owner : String,
  repo : String,
  timestamp : Integer, -- Date.UTC()
  refs_etag : String / false,
  master_sha : String / false,
  release_sha : String / false,
  commits : [CommitInfo, ...],  -- unsorted, only from server
}

RepoInfo = ServerRepoInfo + {
  last_polled : String, -- Date.toISOString()
  branch_day_sha : String,
  commits_map : Map[sha => CommitInfo],
  local_commits : [CommitInfo, ...], -- unsorted, only from github/local
  master_commits : [AnnotatedCommitInfo, ...],
}

CommitInfo = {
  sha : String,
  author : AuthorInfo, committer : AuthorInfo,
  message : String,
  parents : [ { sha : String, _ }, ... ],
}
AuthorInfo = { name: String, email: String, date: String }

AnnotatedCommitInfo = CommitInfo + {
  status_actual : String ("picked" | "no"),
  status_recommend : String ("attn" | "no")
}

LocalRepoInfo = {
  timestamp: Integer, -- Date.UTC()
  refs_etag: String,
  master_sha: String / null,
  release_sha: String / null,
  commits : [CommitInfo, ...] -- unsorted, only commits not in server RepoInfo
}

*/

/* Filesystem:

  data/config.json => Config
  data/repo_{{owner}}_{{repo}} => RepoInfo

*/

/* Local Storage (Cache) -- Data other than plain strings is JSON-encoded

  "repo/{{owner}}/{{repo}}" => LocalRepoInfo

*/

/* ============================================================
   Utilities */

function ok_name(s) {
    return (typeof s === 'string') &&
        /^[_a-zA-Z0-9\-]+$/.test(s);
}

function ok_repo(s) {
    return (typeof s === 'string') &&
        /^([a-zA-Z0-9_\-]+)[\/]([_a-zA-Z0-9\-]+)$/.test(s);
}

function parse_repo(s) {
    var parts = s.split(/[\/]/, 2);
    return {owner: parts[0], repo: parts[1]};
}

function repo_key(owner, repo) {
    return owner + '/' + repo;
}

/* ============================================================
   Ajax */

var cache = {
    config: null,         // Config or null
    repo_info : new Map() // repo_key(owner, repo) => RepoInfo
};

function data_cache_config(k) {
    if (!cache.config) {
        $.ajax({
            url : 'data/config.json',
            dataType : 'json',
            cache : false,
            success : function(data) {
                cache.config = data;
                k(data);
            }
        });
    } else {
        k(cache.config);
    }
}

function data_manager_repos(manager) {
    if (!cache.config) {
        console.log("error: called data_manager_repos before data_cache_config");
        return null;
    } else {
        var repos = cache.config.managers[manager] || [];
        return ($.map(repos, function(repo) { return parse_repo(repo); }));
    }
}

function data_repo_info(owner, repo, k) {
    var key = repo_key(owner, repo);
    if (cache.repo_info.has(key)) {
        k(cache.repo_info.get(key));
    } else {
        $.ajax({
            url : 'data/repo_' + owner + '_' + repo,
            dataType: 'json',
            cache : false,
            success : function(data) {
                augment_repo_info(data);
                load_local_info(data, true);
                cache.repo_info.set(key, data);
                k(data);
            }
        });
    }
}

function load_local_info(ri, loud) {
    var key = repo_key(ri.owner, ri.repo);
    var localri = localStorage.getItem("repo/" + key);
    localri = localri && JSON.parse(localri);
    if (localri && localri.timestamp > ri.timestamp) {
        if (loud) console.log("local information found: ", key);
        ri.timestamp = localri.timestamp;
        ri.refs_etag = localri.refs_etag;
        ri.master_sha = localri.master_sha;
        ri.release_sha = localri.release_sha;
        ri.local_commits = localri.commits;
        augment_repo_info_update(ri);
    } else {
        if (loud && localri) console.log("local information out of date, ignored");
    }
}

function save_local_info(ri) {
    localStorage.setItem('repo/' + repo_key(ri.owner, ri.repo), JSON.stringify({
        timestamp: ri.timestamp,
        refs_etag: ri.refs_etag,
        master_sha: ri.master_sha,
        release_sha: ri.release_sha,
        commits: $.map(ri.local_commits, gh_copy_commit_info)
    }));
}

/* GitHub seems to have buggy Last-Modified / If-Modified-Since handling.
 * I've gotten "notmodified" responses when fetching refs from recently-updated 
 * repos. So let's try ETags or skip it altogether. 
 */

function gh_poll_repo(owner, repo, k) {
    var ri = cache.repo_info.get(repo_key(owner, repo));
    var now = Date.now();
    console.log("github: fetching refs: ", repo_key(owner, repo));
    console.log("  with etag =", ri.refs_etag);
    $.ajax({
        url: 'https://api.github.com/repos/' + owner + '/' + repo + '/git/refs/heads',
        dataType: 'json',
        headers : {
            // "If-Modified-Since": (new Date(ri.timestamp)).toUTCString()
            "If-None-Match": ri.refs_etag || ""
        },
        cache: false,
        success: function(data, status, jqxhr) {
            // console.log("status =", status);
            // console.log("response =", jqxhr.getAllResponseHeaders());
            var etag = jqxhr.getResponseHeader("ETag");
            if (status == "notmodified") {
                console.log("  refs not modified");
                ri.timestamp = now;
                // ri.refs_etag = etag;
                save_local_info(ri);
                augment_repo_info_update(ri);
                k();
            } else {
                gh_update_repo(ri, data, now, etag, k);
            }
        }});
}

function gh_update_repo(ri, data, now, etag, k) {
    // data : [{ref:String, object:{type: ("commit"|?), sha: String}}, ...]
    var master_sha = ri.master_sha, release_sha = ri.release_sha;
    var heads_to_update = [];
    $.each(data, function(index, refinfo) {
        if (refinfo.ref == 'refs/heads/master') {
            if (refinfo.object.sha != master_sha) {
                master_sha = refinfo.object.sha;
                heads_to_update.push(master_sha);
            }
        } else if (refinfo.ref == 'refs/heads/release') {
            if (refinfo.object.sha != release_sha) {
                release_sha = refinfo.object.sha;
                heads_to_update.push(release_sha);
            }
        }
    });
    var new_map = new Map();
    gh_get_commits(ri, heads_to_update, new_map, 0, function() {
        ri.timestamp = now;
        ri.refs_etag = etag;
        ri.master_sha = master_sha;
        ri.release_sha = release_sha;
        new_map.forEach(function(ci, sha) { ri.local_commits.push(ci); });
        save_local_info(ri);
        augment_repo_info_update(ri);
        k();
    });
}

function gh_get_commits(ri, head_shas, new_map, i, k) {
    if (i < head_shas.length) {
        gh_get_commits1(ri, head_shas[i], new_map, function() {
            gh_get_commits(ri, head_shas, new_map, i+1, k);
        });
    } else {
        k();
    }
}

function gh_get_commits1(ri, head_sha, new_map, k) {
    $.ajax({
        url: 'https://api.github.com/repos/' +
            ri.owner + '/' + ri.repo + '/commits?sha=' + head_sha,
        dataType: 'json',
        success: function(data) {
            console.log("github: fetching commits: ", repo_key(ri.owner, ri.repo), head_sha);
            data = $.map(data, function(ci) { return gh_trim_commit_info(ci); });
            var page_map = make_commits_map(data);
            var sha = head_sha;
            while (page_map.has(sha)
                   && !ri.commits_map.has(sha)
                   && !new_map.has(sha)
                   && sha != ri.branch_day_sha) {
                var ci = page_map.get(sha);
                new_map.set(sha, ci);
                if (ci.parents.length == 1) {
                    sha = ci.parents[0].sha;
                } else {
                    ri.error_line = "Merge node at commit " + sha;
                    break;
                }
            }
            if (ri.commits_map.has(sha)
                || new_map.has(sha)
                || sha == ri.branch_day_sha) {
                k();
            } else {
                gh_get_commits1(ri, sha, new_map, k);
            }
        }
    });
}

function gh_trim_commit_info(ci) {
    return {
        sha: ci.sha,
        author: ci.commit.author,
        committer: ci.commit.committer,
        message: ci.commit.message,
        parents: ci.parents
    };
}

function gh_copy_commit_info(ci) {
    return {
        sha: ci.sha,
        author: ci.author,
        committer: ci.committer,
        message: ci.message,
        parents: ci.parents
    };
}

function clear_local_storage() {
    localStorage.clear();
}

/* ============================================================ */

function augment_repo_info(info) {
    info.branch_day_sha = cache.config.branch_day[repo_key(info.owner, info.repo)];
    // FIXME: make timestamp part of ServerRepoInfo
    if (!info.timestamp) info.timestamp = Date.parse(info.last_polled);
    // FIXME: make refs_etag part of ServerRepoInfo
    if (!info.refs_etag) info.refs_etag = null;
    info.commits_map = make_commits_map(info.commits);
    info.error_line = null;  /* may be overridden */
    info.id = make_repo_id(info.owner, info.repo);
    info.todo_id = make_todo_id(info.owner, info.repo);
    info.local_commits = [];
    augment_repo_info_update(info);
}

function augment_repo_info_update(info) {
    info.last_polled = (new Date(info.timestamp)).toISOString();
    $.each(info.local_commits, function(index, ci) {
        info.commits_map.set(ci.sha, ci)
    });
    add_release_map(info);
    add_master_chain(info);
    info.commits_ok = (info.error_line == null);
    info.ncommits = info.master_chain.length
}

function make_commits_map(commits) {
    var map = new Map();
    $.each(commits, function(index, ci) {
        map.set(ci.sha, ci);
    });
    return map;
}

function add_release_map(info) {
    var release_map = new Map();
    var sha = info.release_sha;
    var m;
    while (sha && sha != info.branch_day_sha) {
        release_map.set(sha, "shared");
        var ci = info.commits_map.get(sha);
        var rx = /\(cherry picked from commit ([0-9a-z]*)\)/g;
        while (m = rx.exec(ci.message)) {
            var picked_sha = m[1];
            release_map.set(picked_sha, "picked");
        }
        if (ci.parents.length == 1) {
            sha = ci.parents[0].sha;
        } else {
            info.error_line = "Merge node in release branch at commit " + sha;
            sha = null;
        }
    }
    info.release_map = release_map;
}

function add_master_chain(info) {
    /* sets info.{release_map, master_chain, error_line} */
    var chain = [];
    var sha = info.master_sha;
    while (sha && sha != info.branch_day_sha) {
        var ci = info.commits_map.get(sha);
        chain.push(ci);
        if (ci.parents.length == 1) {
            sha = ci.parents[0].sha;
        } else {
            info.error_line = "Merge node in master branch at commit " + sha;
            info.master_chain = [];
            return;
        }
    }
    chain = chain.reverse();
    $.each(chain, function(index, ci) { augment_commit_info(index, ci, info); });
    info.master_chain = chain;
}

function augment_commit_info(index, info, repo_info) {
    info.id = 'commit_' + info.sha;
    info.status_actual = repo_info.release_map.has(info.sha) ? "picked" : "no";
    info.status_recommend = commit_needs_attention(info) ? "attn" : "no";
    info.class_picked =
        (info.status_actual === "picked") ? "commit_picked" : "commit_unpicked";
    info.class_attn = 
        (info.status_recommend === "attn") ? "commit_attn" : "commit_no_attn";
    info.index = index + 1;
    info.short_sha = info.sha.substring(0,8);
    var lines = info.message.split("\n");
    info.class_one_multi = (lines.length > 1) ? "commit_msg_multi" : "commit_msg_one";
    info.message_line1 = lines[0];
    info.message_rest_lines = $.map(lines.slice(1), function (line) {
        return Handlebars.escapeExpression(line) + '<br/>';
    }).join(" ");
    info.is_picked = (info.status_actual === "picked");
    info.nice_date = info.author.date.substring(0, 4 + 1 + 2 + 1 + 2);
}

function commit_needs_attention(ci) {
    return /merge|release/i.test(ci.message);
}

function make_repo_id(owner, repo) {
    return 'repo_section_' + owner + '_' + repo;
}

function make_todo_id(owner, repo) {
    return 'todo_repo_' + owner + '_' + repo;
}
