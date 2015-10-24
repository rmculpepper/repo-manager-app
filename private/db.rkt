#lang racket/base
(require db
         json
         racket/string
         "github.rkt")
(provide (all-defined-out))

(define the-db-file
  (make-parameter (build-path (find-system-path 'pref-dir) "repo-manager-web-app.db")))

(define the-db
  (virtual-connection
   (lambda () (sqlite3-connect #:database (the-db-file) #:mode 'create))))

;; ============================================================
;; SCHEMA

(define (create-table name fields key-fields)
  (query-exec the-db
    (format "CREATE TABLE ~a (~a, PRIMARY KEY (~a))"
            name
            (string-join fields ", ")
            (string-join key-fields ", "))))

(define (initialize-db)
  ;; Racket repo manager info
  (create-table "managers"
                '("manager TEXT NOT NULL" "owner TEXT NOT NULL" "repo TEXT NOT NULL")
                '("manager" "owner" "repo"))
  (create-table "branch_day"
                '("owner TEXT NOT NULL" "repo TEXT NOT NULL" "sha TEXT NOT NULL")
                '("owner" "repo"))
  ;; Github caches
  (create-table "commits"
                '("owner TEXT NOT NULL" "repo TEXT NOT NULL" "sha TEXT NOT NULL"
                  "json TEXT")
                '("owner" "repo" "sha"))
  (create-table "refs"
                '("owner TEXT NOT NULL" "repo TEXT NOT NULL" "ref TEXT NOT NULL"
                  "json TEXT" "ts INTEGER")
                ;; ts is value of (current-seconds)
                '("owner" "repo" "ref")))

;; ============================================================

(define (db:get-commit owner repo sha #:github? [github? #t])
  (cond [(query-maybe-value the-db
           "SELECT json FROM commits WHERE owner = ? AND repo = ? AND sha = ?"
           owner repo sha)
         => string->jsexpr]
        [(and github? (github:get-commit owner repo sha))
         => (lambda (json)
              (eprintf "Querying github for commit ~a/~a/~a\n" owner repo sha)
              (query-exec the-db
                "INSERT INTO commits (owner, repo, sha, json) VALUES (?, ?, ?, ?)"
                owner repo sha (jsexpr->string json))
              json)]
        [else #f]))

(define (db:get-ref owner repo ref #:github? [github? #t])
  (cond [(query-maybe-value the-db
           "SELECT json FROM refs WHERE owner = ? AND repo = ? AND ref = ?"
           owner repo ref)
         => string->jsexpr]
        [(and github? (github:get-ref owner repo ref))
         => (lambda (json)
              (eprintf "Querying github for ref ~a/~a/~a\n" owner repo ref)
              (query-exec the-db
                "INSERT INTO refs (owner, repo, ref, json, ts) VALUES (?, ?, ?, ?, ?)"
                owner repo ref (jsexpr->string json) (current-seconds))
              json)]
        [else #f]))

(define (db:get-branch owner repo branch)
  (db:get-ref owner repo (format "heads/~a" branch)))
(define (db:get-tag owner repo tag)
  (db:get-ref owner repo (format "tags/~a" tag)))

;; ============================================================

(define (db:get-manager-repos manager)
  (query-rows the-db "SELECT owner, repo FROM managers WHERE manager = ?" manager))

(define (db:create-manager manager owner+repo-list)
  (call-with-transaction the-db
    (lambda ()
      (query-exec the-db "DELETE FROM managers WHERE manager = ?" manager)
      (for ([owner+repo owner+repo-list])
        (query-exec the-db
          "INSERT INTO managers (manager, owner, repo) VALUES (?, ?, ?)"
          manager (car owner+repo) (cadr owner+repo))))))

(define (db:get-branch-day-sha owner repo)
  (query-value the-db "SELECT sha FROM branch_day WHERE owner = ? AND repo = ?" owner repo))
(define (db:set-branch-day-sha owner repo sha)
  (query-value the-db "INSERT INTO branch_day (owner, repo, sha) VALUES (?, ?, ?)" owner repo sha))

;; ============================================================

(define (get-annotated-master-chain owner repo)
  (define master-sha (ref-sha (db:get-branch owner repo "master")))
  (define release-sha
    (cond [(db:get-branch owner repo "release") => ref-sha]
          [else (db:get-branch-day-sha owner repo)]))
  (define merge-base-sha (get-merge-base owner repo master-sha release-sha))
  (define master-chain (get-commit-chain owner repo master-sha merge-base-sha))
  (define release-chain (get-commit-chain owner repo release-sha merge-base-sha))
  (define picked (chain->picked release-chain))
  (annotate-chain master-chain picked))

(define (annotate-chain chain picked)
  (for/list ([ci chain])
    (annotate-commit ci picked)))

(define (annotate-commit ci picked)
  (hash 'info ci
        'status_actual (if (member (commit-sha ci) picked) "picked" "no")
        'status_recommend (if (commit-needs-attention? ci) "attn" "no")))

;; FIXME: add limit
(define (get-merge-base owner repo sha1 sha2)
  (define seen (make-hash))
  (let loop ([sha1 sha1] [sha2 sha2])
    (cond [(hash-ref seen sha1 #f)
           sha1]
          [else
           (hash-set! seen sha1 #t)
           (loop sha2 (commit-parent-sha (db:get-commit owner repo sha1)))])))

;; FIXME: add limit w/ default so we don't run forever
;; returns oldest first
(define (get-commit-chain owner repo start end)
  (let loop ([start start] [chain null])
    (cond [(equal? start end)
           chain]
          [else
           (define ci (db:get-commit owner repo start))
           (loop (commit-parent-sha ci) (cons ci chain))])))

(define (chain->picked chain)
  (filter string? (map commit->picked chain)))

(define (commit->picked ci)
  (define msg (commit-message ci))
  (cond [(regexp-match #rx"\\(cherry picked from commit ([0-9a-z]*)\\)" msg)
         => cadr]
        [else #f]))

(define (commit-needs-attention? ci)
  (regexp-match? #rx"[Mm]erge|[Rr]elease" (commit-message ci)))
