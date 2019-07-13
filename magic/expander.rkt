#lang racket/base

(provide #%top #%top-interaction #%datum #%app)

(require "magic-functions.rkt")
(require racket/stxparam)
(require (for-syntax racket/base syntax/stx syntax/parse))
(require (for-syntax "expander-utils.rkt" "magic-functions.rkt"))

(define-syntax-parameter last-level-offset
  (lambda (stx)
    (raise-syntax-error (syntax-e stx) "can only be used inside any-true?")))

(define-syntax-parameter name-offset 
  (lambda (stx)
    #'0))

;; any-true? is like an or that doesn't short circuit
;; since any-true? indicates the start of a new level we save the current
;; file position to use for relative offsets at this level.
(define-syntax-rule (any-true? body ...)
  (let ([result #f]
        [tmp-offset (file-position (current-input-port))])
    (syntax-parameterize ([last-level-offset (make-rename-transformer #'tmp-offset)])
      ;(printf "any-true: last level offset is ~a~n" last-level-offset)
      (when body (set! result #t))
      ...
      result)))

;; like when but it returns #f instead of #<void> if test expression is false
;; code modified from official when macro 
(define-syntax when*
  (lambda (x)
    (let ([l (syntax->list x)])
      (if (and l
               (> (length l) 2))
          (datum->syntax
           (quote-syntax here)
           (list (quote-syntax if)
                 (stx-car (stx-cdr x))
                 (list*
                  (quote-syntax let-values)
                  (quote-syntax ())
                  (stx-cdr (stx-cdr x)))
                 (quote-syntax #f))
           x)
          (raise-syntax-error
           #f
           "bad syntax"
           x)))))
  
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
     (with-syntax ([name (datum->syntax #'magic-name (string->symbol (syntax->datum #'magic-name)))])
       #'(name off))]
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
    #:datum-literals (leshort lelong beshort belong)
    [(_ (leshort ".s")) #'read-leshort]
    [(_ (lelong ".l")) #'read-lelong]
    [(_ (beshort ".s")) #'read-beshort]
    [(_ (belong ".l")) #'read-belong]
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

(define-syntax (query stx)
  ; investigate syntax->list so that i don't loose src location and other syntax info here
  (let ([lines (cdr (syntax->datum stx))])
    ;(display lines) (printf "~n")
    ;(define lines-syntax-tree (parse-levels lines 0))
    (define lines-syntax-tree (transform-levels 
                               (parse-levels lines 0)))
    (display lines-syntax-tree)
    (printf "~n")
    (datum->syntax stx lines-syntax-tree)))

(define-for-syntax always-true-line '(line (offset 0) (type (default "default")) (test (truetest "x"))))

(define-syntax (named-query stx)
  (syntax-case stx (name-line)
    [(_ (name-line (_ 0) (_ "name") magic-name))
     (with-syntax ([name (datum->syntax #'magic-name (string->symbol (syntax->datum #'magic-name)))])
       #'(define name
           (lambda (new-offset) (void))))]
    [(_ (name-line (_ 0) (_ "name") magic-name) . rst)
     (with-syntax ([name (datum->syntax #'magic-name (string->symbol (syntax->datum #'magic-name)))]
                   [modified-rst (cons (datum->syntax #'rst always-true-line) #'rst)])
       #'(define name
           (lambda (new-offset)
             (syntax-parameterize ([name-offset (make-rename-transformer #'new-offset)]) 
               ;(printf "name: offset = ~a~n" name-offset)
               (query . modified-rst)))))]))
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
           (any-true? #,@(map wrap-with-delimiter-print queries)))
         (provide magic-query magic-query-run-all)))))

(provide
 (except-out (all-from-out racket/base) #%module-begin) 
 (rename-out [magic-module-begin #%module-begin])
 (all-from-out "magic-functions.rkt")
 query line offset reloffset size op disp any-true? when*)

