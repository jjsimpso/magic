#lang racket

(provide #%top #%top-interaction #%datum #%app)

(require "magic-functions.rkt")
(require (for-syntax "expander-utils.rkt"))

#;(define-syntax (query1 stx)
  (syntax-case stx ()
    [(query ...) 
     (let ([q (car (syntax->datum #'(query ...)))])
       (printf "q is ~a~n" q)
       (datum->syntax stx q))]))

#;(define-syntax level
  (syntax-rules ()
    [(level line ...)
     (cond [line #t]
           ...)]
    [_ expr]))

(define-syntax-rule (level line ...)
  (when line ...))

;; todo: switch to `syntax-parse` and use its `#:datum-literals` option or its `~datum` pattern form 
;; to match raw datums without bindings
(define-syntax line
  (syntax-rules (offset type test message)
    [(line (offset off) (type type-expr) (test test-expr)) 
     (magic-test (offset off) (type (quote type-expr) (quote test-expr)) (compare (quote test-expr)))]
    [(line (offset off) (type type-expr) (test test-expr) (message msg)) 
     (magic-test (offset off) (type (quote type-expr) (quote test-expr)) (compare (quote test-expr)) msg)]
    [(_) "no clause found in line"]))

;(define-syntax-rule (offset off)
;  off)

;(define-syntax-rule (type expr)

(define-syntax (query stx)
  ; investigate syntax->list so that i don't loose src location and other syntax info here
  (let ([lines (cdr (syntax->datum stx))])
    ;(display lines)
    ;(define lines-syntax-tree (parse-levels lines 0))
    (define lines-syntax-tree (cons 'when (parse-levels lines 0)))
    (display lines-syntax-tree)
    (datum->syntax stx lines-syntax-tree)))

(define-syntax-rule (magic-module-begin (magic QUERY ...))
  (#%module-begin QUERY ...))

(provide (rename-out [magic-module-begin #%module-begin])
         (all-from-out "magic-functions.rkt")
         query line level when)

