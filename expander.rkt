#lang racket

(provide #%top #%top-interaction #%datum #%app)

(define-syntax (query ...)
  42)

(define-syntax-rule (magic-module-begin (magic QUERY ...))
  (#%module-begin QUERY ...))

(provide (rename-out [magic-module-begin #%module-begin])
         query)




;; sample walk of a tree of tests
(define (line1) #t)
(define (line2) #f)
(define (line3) #t)
(define (line4) #f)
(define (line5) #f)

(define test-exp `((,line1 ((,line2) (,line3 ((,line4) (,line5)))))))
(define test-exp2 `((,line1) (,line2) (,line3)))

(define (test-walk exp)
  (cond [(null? exp) #f]
        [(not (pair? exp)) #f]
        [(list? (car exp)) 
         (let ([result (test-walk (car exp))])
           (if (eq? result 'match)
               'match
               (test-walk (cdr exp))))]
        [((car exp))
         (printf "matched ~a~n" (car exp))
         (if (null? (cdr exp))
             'match
             (test-walk (cdr exp)))]
        [else #f]))
