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

(require racket/port)
(require racket/string)
(require racket/stxparam)
(require "magic-functions.rkt")
(require "expander-utils.rkt")
(require "output.rkt")

(require (for-syntax racket/base syntax/stx syntax/parse racket/syntax))
(require (for-syntax "expander-utils.rkt" "magic-functions.rkt" "output.rkt"))

(define-syntax-parameter name-offset 
  (lambda (stx)
    #'0))

;; todo: switch to `syntax-parse` and use its `#:datum-literals` option or its `~datum` pattern form 
;; to match raw datums without bindings. when i do this i should consider replacing type and compare
;; with macros.
(define-syntax (line stx)
  (syntax-case stx (offset reloffset relindoff type test message)
    [(line (offset off) (type (_ "use")) (test (_ magic-name)))
     #'(magic-name (use-offset off))]
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

;; this macro is used as a replacement for the standard offset macro in 'use' lines
;; file is either doing something subtle with indirect offsets or it treats use differently
;; than the other tests. in the case of indirect offsets we need to add the name-offset to
;; both the location to read from and the resulting offset. but only for use lines. this
;; change breaks other types of tests. this problem was found when i incorporated the
;; jpeg magic from file.
;; btw, according to the man page, indirect offsets in names are always relative to
;; the start of the file and ignore the name's offset, unlike absolute offsets which are
;; adjusted by the name's offset. the wording isn't precise but this appears to be wrong.
;; indirect offsets are definitely read from a location relative to the name's offset. and
;; in the case of use the name's offset is also applied to the result.
(define-syntax (use-offset stx)
  (syntax-parse stx
    [(_ off:integer)
     #'(+ name-offset off)]
    ;; indirect offset
    [(_ (indoff off:integer (~optional size-expr:expr) (~optional (~seq op-expr disp-expr))))
     #'(+ (indoff (+ name-offset off) (~? size-expr) (~? op-expr) (~? disp-expr))
          name-offset)]
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
    [(_ (byte ".b")) #'(lambda () (read-byt #t))]
    [(_ (byte ".B")) #'(lambda () (read-byt #t))]
    [(_ (byte ".c")) #'(lambda () (read-byt #t))]
    [(_ (byte ".C")) #'(lambda () (read-byt #t))]
    [(_ (leshort ".s")) #'(lambda () (read-leshort #t))]
    [(_ (beshort ".S")) #'(lambda () (read-beshort #t))]
    [(_ (lelong  ".l")) #'(lambda () (read-lelong #t))]
    [(_ (belong  ".L")) #'(lambda () (read-belong #t))]
    [(_ (ubyte ",b")) #'read-byt]
    [(_ (ubyte ",B")) #'read-byt]
    [(_ (ubyte ",c")) #'read-byt]
    [(_ (ubyte ",C")) #'read-byt]
    [(_ (uleshort ",s")) #'read-leshort]
    [(_ (ubeshort ",S")) #'read-beshort]
    [(_ (ulelong  ",l")) #'read-lelong]
    [(_ (ubelong  ",L")) #'read-belong]
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

(define-syntax (disp stx)
  (syntax-parse stx
    [(_ val:integer) #'val]
    [(_ (memvalue val:integer))
     #'(lambda (read-func base-offset)
         (file-position (current-input-port) (+ base-offset val))
         (read-func))]))
     ;(error (format "line ~a: memvalue not implemented" (syntax-line stx)))]))

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
                 (print-info "~a offset = 0x~a~n" magic-name (number->string name-offset 16))
                 (query . modified-rst))))
           (define ^magic-name
             (lambda (new-offset)
               (syntax-parameterize ([name-offset (make-rename-transformer #'new-offset)])
                 (print-info "~a offset = 0x~a~n" ^magic-name (number->string name-offset 16))
                 (query #,@(reverse-endianness #'modified-rst)))))))]))

(struct magic-result
  (output-text
   strength
   mime-type
   ext)
  #:prefab)

(define (replace-query-failed-w-false result)
  (if (equal? result #s(magic-result "" 0 "" ""))
      #f
      result))

;; 
(define-syntax (wrap-queries-for-run-all stx)
  (syntax-parse stx
    [(_ (qry ...))
     #'(filter struct?
               (list (replace-query-failed-w-false
                      (magic-result
                       (with-output-to-string
                         (lambda () qry))
                       0
                       ""
                       "")) ...))]
    [_ (error "wrap-queries format error")]))

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
  
  (let ([exprs (cdadr (syntax->datum stx))])
    ;(display queries)
    (let ([queries (filter query? exprs)]
          [named-queries (filter named-query? exprs)])
      #`(#%module-begin
         #,@named-queries
         (define (magic-query-thunk)
           (or #,@queries))
         
         (define (magic-query)
           (let ([result (with-output-to-string magic-query-thunk)])
             (if (non-empty-string? result)
                 (magic-result result 0 "" "")
                 #f)))

         (define (magic-query-run-all)
           (wrap-queries-for-run-all #,queries))
         
         (provide (struct-out magic-result) magic-query magic-query-run-all)))))

(provide
 (except-out (all-from-out racket/base) #%module-begin) 
 (rename-out [magic-module-begin #%module-begin])
 (all-from-out "magic-functions.rkt" racket/port)
 (struct-out magic-result) query line clear-line offset reloffset size op disp any-true? begin-level when*)
