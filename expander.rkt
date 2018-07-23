#lang racket

(provide #%top #%top-interaction #%datum #%app)

;(define-macro (bf-module-begin PARSE-TREE)
;  #'(#%module-begin
;     'PARSE-TREE))

(define-syntax-rule (bf-module-begin e ...)
  (#%module-begin e ...))

(provide (rename-out [bf-module-begin #%module-begin]))

(define (fold-funcs apl bf-funcs)
  (for/fold ([current-apl apl])
            ([bf-func (in-list bf-funcs)])
    (apply bf-func current-apl)))

;(define-macro (bf-program OP-OR-LOOP-ARG ...)
;  #'(begin
;      (define first-apl (list (make-vector 30000 0) 0))
;      (void (fold-funcs first-apl (list OP-OR-LOOP-ARG ...)))))
(define-syntax-rule (bf-program OP-OR-LOOP-ARG ...)
  (begin
    (define first-apl (list (make-vector 30000 0) 0))
    (void (fold-funcs first-apl (list OP-OR-LOOP-ARG ...)))))
(provide bf-program)

;(define-macro (bf-loop "[" OP-OR-LOOP-ARG ... "]")
;  #'(lambda (arr ptr)
;      (for/fold ([current-apl (list arr ptr)])
;                ([i (in-naturals)]
;                 #:break (zero? (apply current-byte
;                                       current-apl)))
;        (fold-funcs current-apl (list OP-OR-LOOP-ARG ...)))))
(define-syntax-rule (bf-loop "[" OP-OR-LOOP-ARG ... "]")
  (lambda (arr ptr)
    (for/fold ([current-apl (list arr ptr)])
              ([i (in-naturals)]
               #:break (zero? (apply current-byte
                                     current-apl)))
      (fold-funcs current-apl (list OP-OR-LOOP-ARG ...)))))
(provide bf-loop)

;(define-macro-cases bf-op
;  [(bf-op ">") #'gt]
;  [(bf-op "<") #'lt]
;  [(bf-op "+") #'plus]
;  [(bf-op "-") #'minus]
;  [(bf-op ".") #'period]
;  [(bf-op ",") #'comma])
(define-syntax (bf-op caller-stx)
  (syntax-case caller-stx ()
    [(bf-op ">") #'gt]
    [(bf-op "<") #'lt]
    [(bf-op "+") #'plus]
    [(bf-op "-") #'minus]
    [(bf-op ".") #'period]
    [(bf-op ",") #'comma]))
(provide bf-op)

(define (current-byte arr ptr) (vector-ref arr ptr))

(define (set-current-byte arr ptr val)
  (define new-arr (vector-copy arr))
  (vector-set! new-arr ptr val)
  new-arr)

(define (gt arr ptr) (list arr (add1 ptr)))
(define (lt arr ptr) (list arr (sub1 ptr)))

(define (plus arr ptr)
  (list
   (set-current-byte arr ptr (add1 (current-byte arr ptr)))
   ptr))

(define (minus arr ptr)
  (list
   (set-current-byte arr ptr (sub1 (current-byte arr ptr)))
   ptr))

(define (period arr ptr)
  (write-byte (current-byte arr ptr))
  (list arr ptr))

(define (comma arr ptr)
  (list (set-current-byte arr ptr (read-byte)) ptr))
