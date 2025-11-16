#lang racket/base
(require (rename-in racket/match [match-define defmatch])
         racket/list
         racket/runtime-path
         racket/date
         racket/string
         racket/promise
         json
         "private/github.rkt")
(provide (all-defined-out))

(define DFS-FUEL 500)
(define BFS-FUEL 500)

(define-runtime-path data-dir "web-content/data")
(define-runtime-path config-file "web-content/data/base.json")

(define config (call-with-input-file* config-file read-json))
(define branch-day-map (hash-ref config 'branch_day '#hash()))

;; ========================================

;; update caches for all repos
(define (update)
  (for ([o/r (in-hash-keys branch-day-map)])
    (update1 o/r)))

(define (update1 o/r)
  (with-handlers ([exn:fail?
                   (λ (exn) (eprintf "update for repository ~v failed. \nmessage: ~v\nContinuing.\n"
                                     o/r
                                     (exn-message exn)))])
    (define ts (* 1000 (current-seconds)))
    (defmatch (list owner repo) (string-split (symbol->string o/r) "/"))
    (define branch-day-sha (hash-ref branch-day-map o/r))
    (defmatch (list master-sha release-sha stable-sha refs-etag) (get-certain-refs+etag owner repo))
    (define out-file (build-path data-dir (format "repo_~a_~a.json" owner repo)))
    (define cache (make-hash))
    ;; We want commits = { ^branch-day master release } exactly.
    ;; (N.B. this notation is standard git notation, documented at
    ;;   That notation (without the braces, though) is one of git's ways of
    ;;   https://git-scm.com/book/en/v2/Git-Tools-Revision-Selection in the
    ;;   "Commit Ranges" section. In this case: all ancestors of master & release, except for
    ;;   any ancestors of branch-day.
    ;; 1. build a cache containing at least all commits in { ^branch-day master release }
    (add-commits-from-file cache out-file)
    (when master-sha
      (or (fetch-commits owner repo master-sha branch-day-sha cache)
          (fetch-commits/dominator owner repo master-sha branch-day-sha stable-sha cache)))
    (when release-sha
      (or (fetch-commits owner repo release-sha branch-day-sha cache)
          (fetch-commits/dominator owner repo release-sha branch-day-sha stable-sha cache)))
    ;; 2. remove everything reachable from branch-day
    (remove-commits branch-day-sha cache)
    ;; 3. finally, copy over everything reachable from { master release }
    ;; (in case cache had random unrelated commits not reachable from branch-day)
    (define commits (make-hash))
    (when master-sha (add-commits master-sha commits cache))
    (when master-sha (add-commits release-sha commits cache))
    (define repo-info
      (hash 'owner owner
            'repo repo
            'timestamp ts
            'refs_etag refs-etag
            'master_sha master-sha
            'release_sha release-sha
            'commits (hash-values commits)))
    (printf "writing ~s\n" (path->string out-file))
    (call-with-output-file* out-file
                            #:exists 'truncate/replace
                            (lambda (o) (write-json repo-info o)))))

;; return the commit-shas of the heads of certain branches and also
;; the etag of the request
(define (get-certain-refs+etag owner repo)
  (defmatch (cons info etag) (github:get-refs+etag owner repo))
  (define h
    (for/hash ([refinfo (in-list info)])
      (values (hash-ref refinfo 'ref) (hash-ref* refinfo 'object 'sha))))
  (list (hash-ref h "refs/heads/master" #f)
        (hash-ref h "refs/heads/release" #f)
        (hash-ref h "refs/heads/stable" #f)
        etag))

(define (add-commits-from-file commits out-file)
  (let ([old-repo-info
         (with-handlers ([exn:fail? (lambda _ #f)])
           (call-with-input-file* out-file read-json))])
    (when old-repo-info
      (for ([ci (in-list (hash-ref old-repo-info 'commits null))])
        (hash-set! commits (hash-ref ci 'sha) ci)))))

;; fetch-commits : ... -> boolean
;; Attempts to add all commits base-sha..start-sha; may fail if branch bypasses base-sha.
;; Returns #t if all chains terminated at base-sha, #f if any out of fuel
(define (fetch-commits owner repo start-sha base-sha cache)
  (define visited (make-hash))
  (let loop ([sha start-sha] [fuel DFS-FUEL])
    (cond [(equal? sha base-sha) #t]
          [(hash-ref visited sha #f) #t]
          [(zero? fuel) (eprintf "*** dfs out of fuel\n") #f]
          [else
           (hash-set! visited sha #t)
           (define ci (or (get-commit owner repo sha cache) #hash()))
           (for/and ([parent (hash-ref ci 'parents null)])
             (loop (hash-ref parent 'sha) (sub1 fuel)))])))

;; Add all commits in base-sha..start-sha (and more) by searching for a dominator.
;; ignore any references to the stable-sha
(define (fetch-commits/dominator owner repo start-sha base-sha stable-sha cache)
  (define visited (make-hash))
  (eprintf "*** finding dominator for ~a/~a\n" owner repo)
  ;; Invariant: every path from some node in (list start-sha base-sha) to root
  ;; goes through some node in the worklist.
  (define initial-worklist (list start-sha base-sha))
  (when (member stable-sha initial-worklist)
    (error 'fetch-commits/dominator "either start-sha (~v) or base-sha (~v) is equal to stable-sha (~v). Maybe a problem?"
           start-sha base-sha stable-sha))
  (let loop ([worklist initial-worklist] [fuel BFS-FUEL])
    (when (zero? fuel)
      (error 'update "bfs out of fuel: ~a/~a" owner repo))
    ;; We're done when the worklist is a single node.
    (when (> (length worklist) 1)
      (let* ([worklist (remove* (list stable-sha) (remove-duplicates worklist))]
             [cis (for/list ([sha worklist]
                             #:when (not (hash-ref visited sha #f)))
                    (hash-set! visited sha #t)
                    (get-commit owner repo sha cache))])
        (loop (for*/list ([ci cis] [parent (hash-ref ci 'parents null)]) (hash-ref parent 'sha))
              (sub1 fuel))))))

(define (get-commit owner repo sha cache)
  (or (hash-ref cache sha #f)
      (let ([commits (github:get-commits owner repo sha)])
        (for ([ci (in-list commits)])
          (hash-set! cache (hash-ref ci 'sha) ci))
        (hash-ref cache sha))))

(define (remove-commits sha cache)
  (let loop ([sha sha])
    (define ci (hash-ref cache sha #f))
    (when ci
      (hash-remove! cache sha)
      (for ([parent (hash-ref ci 'parents null)])
        (loop (hash-ref parent 'sha))))))

(define (add-commits sha commits cache)
  (let loop ([sha sha])
    (when (not (hash-has-key? commits sha))
      (define ci (hash-ref cache sha #f))
      (when ci
        (hash-set! commits sha ci)
        (for ([parent (hash-ref ci 'parents null)])
          (loop (hash-ref parent 'sha)))))))

;; ========================================

(module+ main
  (printf "Updating\n")
  (update))
