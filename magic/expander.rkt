#lang racket/base

(provide #%top #%top-interaction #%datum #%app)

(require "magic-functions.rkt")
(require (for-syntax racket/base syntax/stx syntax/parse))
(require (for-syntax "expander-utils.rkt" "magic-functions.rkt"))

(define-syntax (any-true? stx)
  (syntax-parse stx
    [(_ body ...)
     #'((let ([result #f])
          (when body (set! result #t))
          ...))]))

;; this is not correct!!
(define-syntax-rule (level line ...)
  (when line ...))
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
(define-syntax line
  (syntax-rules (offset type test message)
    [(line (offset off) (type type-expr) (test test-expr)) 
     (magic-test (offset off) (type (quote type-expr) (quote test-expr)) (compare (quote test-expr)))]
    [(line (offset off) (type type-expr) (test test-expr) (message msg)) 
     (magic-test (offset off) (type (quote type-expr) (quote test-expr)) (compare (quote test-expr)) msg)]
    [(_) "no clause found in line"]))

;(define-syntax-rule (offset off)
;  off)

(define-syntax (size stx)
  (syntax-parse stx
    #:datum-literals (leshort lelong)
    [(_ (leshort ".s")) #'read-leshort]
    [(_ (lelong ".l")) #'read-lelong]))

;(define-syntax-rule (type expr)

(define-syntax (query stx)
  ; investigate syntax->list so that i don't loose src location and other syntax info here
  (let ([lines (cdr (syntax->datum stx))])
    ;(display lines)
    ;(define lines-syntax-tree (parse-levels lines 0))
    (define lines-syntax-tree (transform-levels 
                               (parse-levels lines 0)))
    (display lines-syntax-tree)
    (printf "~n")
    (datum->syntax stx lines-syntax-tree)))

(define-syntax-rule (magic-module-begin (magic QUERY ...))
  (#%module-begin 
   (define (magic-query) 
     QUERY ...)
   (provide magic-query)))

(provide
 (except-out (all-from-out racket/base) #%module-begin) 
 (rename-out [magic-module-begin #%module-begin])
 (all-from-out "magic-functions.rkt")
 query line level size)

