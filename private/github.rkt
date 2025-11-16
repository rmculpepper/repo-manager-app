#lang racket/base
(require racket/string
         net/head
         json
         "net.rkt"
         "github-base.rkt")
(provide (all-defined-out))

(define loud? #f)

(define (gh-endpoint . args)
  (string-join (cons "https://api.github.com" args) "/"))

;; ============================================================
;; COMMITS (https://developer.github.com/v3/git/commits/)

;; Get a commit
(define (github:get-commit owner repo sha)
  (when loud? (eprintf "github:get-commit ~s ~s ~s\n" owner repo sha))
  (get/github (gh-endpoint "repos" owner repo "git/commits" sha)))
;; CommitInfo = {
;;   sha : String,
;;   author : AuthorInfo, committer : AuthorInfo,
;;   message : String,
;;   parents : [ { sha : String, _ }, ... ],
;;   _ }
;; AuthorInfo = { date = String, name = String, email = String }

(define (github:get-commits owner repo sha)
  (when loud? (eprintf "github:get-commits ~s ~s ~s\n" owner repo sha))
  (map (lambda (ci)
         (hash 'sha (hash-ref* ci 'sha)
               'parents (hash-ref* ci 'parents)
               'author (hash-ref* ci 'commit 'author)
               'message (hash-ref* ci 'commit 'message)))
       (get/github
        (url-add-query (gh-endpoint "repos" owner repo "commits")
                       `([sha . ,sha])))))
;; [{sha, commit: {author, message}, parents}, ...]

;; ============================================================
;; REFERENCES (https://developer.github.com/v3/git/refs/)

;; Get a reference
(define (github:get-ref owner repo ref)
  (when loud? (eprintf "github:get-ref ~s ~s ~s\n" owner repo ref))
  (get/github (gh-endpoint "repos" owner repo "git/refs" ref)
              #:fail (lambda _ #f)))
;; RefInfo = { ref : String, object : { type : "commit", sha : String, _ }, _ }

(define (github:get-refs+etag owner repo)
  (when loud? (eprintf "github:get-refs+etag ~s ~s\n" owner repo))
  (get/github (gh-endpoint "repos" owner repo "git/refs/heads")
              #:handle2
              (lambda (in headers)
                (define etag (extract-field "ETag" headers))
                (define info (read-json in))
                (cons info etag))))

(define (ref-sha ri) (hash-ref* ri 'object 'sha))

;; ============================================================
;; Utils

(define (hash-ref* h . ks)
  (for/fold ([h h]) ([k ks]) (and h (hash-ref h k #f))))
