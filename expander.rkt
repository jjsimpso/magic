#lang racket/base

;;Copyright 2019 Jonathan Simpson
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.

(provide #%top #%top-interaction #%datum #%app)

(require "magic-functions.rkt")
(require racket/stxparam)
(require (for-syntax racket/base syntax/stx syntax/parse racket/syntax))
(require (for-syntax "expander-utils.rkt" "magic-functions.rkt"))
(require "expander-utils.rkt")

(define-syntax-parameter name-offset 
  (lambda (stx)
    #'0))

#|
(define-syntax (level stx)
  (syntax-parse stx
    #:datum-literals (level)
    [(_ (line:expr (level ...))) #'(when line (level ...))]
    [(_ (line:expr ...)) #']))
|#
;; todo: switch to `syntax-parse` and use its `#:datum-literals` option or its `~datum` pattern form 
;; to match raw datums without bindings. when i do this i should consider replacing type and compare
;; with macros.
#;(define-syntax line
  (syntax-rules (offset type test message)
    [(line (offset 0) (type (_ "use")) (test (_ name)))
     (name)]
    [(line (offset off) (type type-expr) (test test-expr)) 
     (magic-test (offset off) (type (quote type-expr) (quote test-expr)) (compare (quote test-expr)))]
    [(line (offset off) (type type-expr) (test test-expr) (message msg)) 
     (magic-test (offset off) (type (quote type-expr) (quote test-expr)) (compare (quote test-expr)) msg)]
    [(_) "no clause found in line"]))

(define-syntax (line stx)
  (syntax-case stx (offset reloffset relindoff type test message)
    [(line (offset off) (type (_ "use")) (test (_ magic-name)))
     #'(magic-name (offset off))]
    ; match relative offsets so that we can bypass the offset macro, which conflicts with named queries
    [(line (offset (reloffset off)) (type type-expr) (test test-expr)) 
     (syntax-protect #'(magic-test (reloffset off) (type (quote type-expr) (quote test-expr)) (compare (quote test-expr) (quote type-expr))))]
    [(line (offset (relindoff off)) (type type-expr) (test test-expr)) 
     (syntax-protect #'(magic-test (relindoff off) (type (quote type-expr) (quote test-expr)) (compare (quote test-expr) (quote type-expr))))]
    [(line (offset off) (type type-expr) (test test-expr)) 
     (syntax-protect #'(magic-test (offset off) (type (quote type-expr) (quote test-expr)) (compare (quote test-expr) (quote type-expr))))]
    ; match relative offsets so that we can bypass the offset macro, which conflicts with named queries
    [(line (offset (reloffset off)) (type type-expr) (test test-expr) (message msg)) 
     (syntax-protect #'(magic-test (reloffset off) (type (quote type-expr) (quote test-expr)) (compare (quote test-expr) (quote type-expr)) msg))]
    [(line (offset (relindoff off)) (type type-expr) (test test-expr) (message msg)) 
     (syntax-protect #'(magic-test (relindoff off) (type (quote type-expr) (quote test-expr)) (compare (quote test-expr) (quote type-expr)) msg))]
    [(line (offset off) (type type-expr) (test test-expr) (message msg)) 
     (syntax-protect #'(magic-test (offset off) (type (quote type-expr) (quote test-expr)) (compare (quote test-expr) (quote type-expr)) msg))]
    [(_) #'"no clause found in line"]))

(define-syntax (clear-line stx)
  (syntax-case stx (offset)
    [(clear-line (offset off) "clear")
     #'(test-passed-at-level #f)]))

;; in named queries, absolute offsets are relative to the argument of the query.
;; so add that offset here, which will be zero for regular queries.
;; because of this, relative offsets in named queries can't use this macro, since the last-level-offset
;; will have already taken the name-offset into account.
(define-syntax-rule (offset off)
  (+ name-offset off))

(define-syntax-rule (reloffset off)
  (+ last-level-offset off))

(define-syntax (size stx)
  (syntax-parse stx
    #:datum-literals (byte leshort lelong beshort belong)
    [(_ (byte ".b")) #'read-byt]
    [(_ (byte ".B")) #'read-byt]
    [(_ (byte ".c")) #'read-byt]
    [(_ (byte ".C")) #'read-byt]
    [(_ (leshort ".s")) #'read-leshort]
    [(_ (beshort ".S")) #'read-beshort]
    [(_ (lelong  ".l")) #'read-lelong]
    [(_ (belong  ".L")) #'read-belong]
    ))

(define-syntax (op stx)
  (syntax-parse stx
    [(_ "*") #'*]
    [(_ "+") #'+]
    [(_ "-") #'-]
    [(_ "/") #'/]
    [(_ "%") #'remainder]
    [(_ "&") #'bitwise-and]
    [(_ "|") #'bitwise-ior]
    [(_ "^") #'bitwise-xor]
    ))

(define-syntax-rule (disp arg)
  arg)

;(define-syntax-rule (type expr)

#;(define-syntax (query stx)
  ; investigate syntax->list so that i don't loose src location and other syntax info here
  (let ([lines (cdr (syntax->datum stx))])
    ;(display lines) (printf "~n")
    ;(define lines-syntax-tree (parse-levels lines 0))
    (define lines-syntax-tree (transform-levels 
                               (parse-levels lines 0)))
    (display lines-syntax-tree)
    (printf "~n")
    (datum->syntax stx lines-syntax-tree)))

(define-syntax (query stx)
  (printf "~a~n" stx)
  (syntax-parse stx
    [(_ expr ...)
     #'(parse-level0 expr ...)]))

(define-for-syntax always-true-line '(line (offset 0) (type (numeric "byte")) (test (truetest "x"))))

(define (swap-endianness item)
  (cond
    [(pair? item) 
     (cons (swap-endianness (car item))
           (swap-endianness (cdr item)))]
    [(and (string? item) (string=? item "beshort")) "leshort"]
    [(and (string? item) (string=? item "belong")) "lelong"]
    [(and (string? item) (string=? item "bequad")) "lequad"]
    [(and (string? item) (string=? item "befloat")) "lefloat"]
    [(and (string? item) (string=? item "bedouble")) "ledouble"]

    [(and (string? item) (string=? item "leshort")) "beshort"]
    [(and (string? item) (string=? item "lelong")) "belong"]
    [(and (string? item) (string=? item "lequad")) "bequad"]
    [(and (string? item) (string=? item "lefloat")) "befloat"]
    [(and (string? item) (string=? item "ledouble")) "bedouble"]

    [else item]))

(define-syntax (named-query stx)
  (syntax-case stx (name-line)
    [(_ (name-line (_ 0) (_ "name") magic-name))
     (with-syntax ([^magic-name (format-id stx "^~a" (syntax-e #'magic-name))])
       #'(begin
           (define magic-name
             (lambda (new-offset) (void)))
           (define ^magic-name
             (lambda (new-offset) (void)))))]
    [(_ (name-line (_ 0) (_ "name") magic-name) . rst)
     (with-syntax ([^magic-name (format-id stx "^~a" (syntax-e #'magic-name))]
                   [modified-rst (cons (datum->syntax #'rst always-true-line) #'rst)])
       #`(begin 
           (define magic-name
             (lambda (new-offset)
               (syntax-parameterize ([name-offset (make-rename-transformer #'new-offset)]) 
                 (eprintf "~a offset = ~a~n" magic-name name-offset)
                 (query . modified-rst))))
           (define ^magic-name
             (lambda (new-offset)
               (syntax-parameterize ([name-offset (make-rename-transformer #'new-offset)]) 
                 ;(printf "name: offset = ~a~n" name-offset)
                 (query #,@(reverse-endianness #'modified-rst)))))))]))
               
  ;#'(query #,stx))
  ;#'(void))

#;(define-syntax-rule (magic-module-begin (magic QUERY ...))
  (#%module-begin 
   (define (magic-query) 
     (or QUERY ...))
   (provide magic-query)))

#;(define-syntax (magic-module-begin stx)
  (define (query? expr)
    (if (and (pair? expr) 
             (equal? (car expr) 'query))
        #t
        #f))
  (define (named-query? expr)
    (if (and (pair? expr)
             (equal? (car expr) 'named-query))
        #t
        #f))
  (syntax-parse stx
    #:datum-literals (magic)
    [(_ (magic expr ...))
        #'(#%module-begin
           (define (magic-query) 
             (or expr ...))
           (provide magic-query))]))

(define-syntax (magic-module-begin stx)
  (define (query? expr)
    (if (and (pair? expr) 
             (equal? (car expr) 'query))
        #t
        #f))
  (define (named-query? expr)
    (if (and (pair? expr)
             (equal? (car expr) 'named-query))
        #t
        #f))
  (define (wrap-with-delimiter-print expr)
    (list 'when* expr '(printf "*** ")))

  (let ([exprs (cdadr (syntax->datum stx))])
    ;(display queries)
    (let ([queries (filter query? exprs)]
          [named-queries (filter named-query? exprs)])
      #`(#%module-begin 
         #,@named-queries
         (define (magic-query)
           (or #,@queries))
         (define (magic-query-run-all)
           ; any-true? creates a binding for last-level-offset which we probably don't want here. investigate.
           (any-true? #,@(map wrap-with-delimiter-print queries)))
         (provide magic-query magic-query-run-all)))))

(provide
 (except-out (all-from-out racket/base) #%module-begin) 
 (rename-out [magic-module-begin #%module-begin])
 (all-from-out "magic-functions.rkt")
 query line clear-line offset reloffset size op disp any-true? begin-level when*)

