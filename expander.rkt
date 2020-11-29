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
(require (for-syntax "expander-utils.rkt" "magic-functions.rkt" "output.rkt"))
(require "expander-utils.rkt")
(require "output.rkt")

(define-syntax-parameter name-offset 
  (lambda (stx)
    #'0))

;; todo: switch to `syntax-parse` and use its `#:datum-literals` option or its `~datum` pattern form 
;; to match raw datums without bindings. when i do this i should consider replacing type and compare
;; with macros.
(define-syntax (line stx)
  (syntax-case stx (offset reloffset relindoff type test message)
    [(line (offset off) (type (_ "use")) (test (_ magic-name)))
     #'(magic-name (offset off))]
    [(line (offset off) (type type-expr) (test test-expr)) 
     (syntax-protect #'(magic-test (offset off) (type (quote type-expr) (quote test-expr)) (compare (quote test-expr) (quote type-expr))))]
    [(line (offset off) (type type-expr) (test test-expr) (message msg)) 
     (syntax-protect #'(magic-test (offset off) (type (quote type-expr) (quote test-expr)) (compare (quote test-expr) (quote type-expr)) msg))]
    [(_) #'"no clause found in line"]))

(define-syntax (clear-line stx)
  (syntax-case stx (offset)
    [(clear-line (offset off) "clear" (test test-expr))
     #'(test-passed-at-level #f)]
    [(clear-line (offset off) "clear")
     #'(test-passed-at-level #f)]))

;; in named queries, absolute offsets are relative to the argument of the query.
;; so add that offset here, which will be zero for regular queries.
;; for relative offsets in named queries, the last-level-offset will have already taken
;; the name-offset into account(still need to verify relative offsets in named queries).
(define-syntax (offset stx)
  (syntax-parse stx
    [(_ off:integer)
     #'(+ name-offset off)]
    ;; indirect offset
    [(_ (indoff off:integer (~optional size-expr:expr) (~optional (~seq op-expr disp-expr))))
     #'(indoff (+ name-offset off) (~? size-expr) (~? op-expr) (~? disp-expr))]
    ;; indirect relative offset
    [(_ (indoff (reloffset off:integer) (~optional size-expr:expr) (~optional (~seq op-expr disp-expr))))
     #'(indoff (reloffset off) (~? size-expr) (~? op-expr) (~? disp-expr))]
    ;; relative offset
    [(_ (reloffset off:integer))
     #'(reloffset off)]
    ;; relative indirect offset
    [(_ (relindoff off:expr))
     ; still need to test this in a named query
     #'(reloffset (offset off))]))

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

(define-syntax (query stx)
  (syntax-parse stx
    [(_ expr ...)
     #'(parse-level0 expr ...)]))

(define-for-syntax always-true-line '(line (offset 0) (type (numeric "byte")) (test (truetest "x"))))

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
                 (print-debug "~a offset = 0x~a~n" magic-name (number->string name-offset 16))
                 (query . modified-rst))))
           (define ^magic-name
             (lambda (new-offset)
               (syntax-parameterize ([name-offset (make-rename-transformer #'new-offset)])
                 (print-debug "~a offset = 0x~a~n" ^magic-name (number->string name-offset 16))
                 (query #,@(reverse-endianness #'modified-rst)))))))]))

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

