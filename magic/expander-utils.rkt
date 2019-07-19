#lang racket

(provide parse-levels)
(provide transform-levels)

(define (eat-lines-with-greater-level lines level)
  (define next-line-level
    (for/fold ([level 0])
              ([line lines])
      #:break (not (and (pair? line)
                        (equal? (car line) 'level)))         
          (add1 level)))

  ;; drop any level lines from the lines list
  (define next-lines (list-tail lines next-line-level))

  ;(printf "nll = ~a, next lines = ~a~n" next-line-level next-lines)

  (cond 
    [(> next-line-level level)
     (eat-lines-with-greater-level (cdr next-lines) level)]
    [else
     lines]))

;; ex: (parse-levels '((line1) (level) (line2) (level) (level) (line3) (level) (line4) (level) (level) (line5)) 0)  
(define (parse-levels lines level)
  ;; count level of next line
  (define next-line-level
    (for/fold ([level 0])
              ([line lines])
      #:break (not (and (pair? line)
                        (equal? (car line) 'level)))         
          (add1 level)))
  
  ;; drop any level lines from the lines list
  (define next-lines (list-tail lines next-line-level))
  
  (cond
    [(null? lines) '()]
    [(= next-line-level level)
     (cons (car next-lines)
           (parse-levels (cdr next-lines) level))]
    [(= next-line-level (add1 level))
     (cons (cons 'level 
                 (cons (car next-lines) 
                       (parse-levels (cdr next-lines) (add1 level))))
           (parse-levels (eat-lines-with-greater-level lines level) level))]
    [(< next-line-level level) '()]))

(define (level-tree-to-code tree)
  #f)


;; transforms code from something like this:
;; ((line1)
;;  (level) (line2)
;;          (line3)
;;          (level (line4)
;;                 (line5))))
;;
;; to this:
;; (when (line1)
;;   (begin
;;     (line2)
;;     (when (line3)
;;       (begin
;;         (line4)
;;         (line5)))))
;;
;; test: (transform-levels '((line1) (level (line2) (line3) (level (line4) (line5)))))
;; test: (transform-levels '((line1) (level (line2))))
(define (transform-levels tree)
  (define (transform-levels-helper tree)
    (cond 
      [(null? tree) '()]
      [(and (list? (cdr tree))
            (not (empty? (cdr tree)))
            (eq? (caadr tree) 'level))
       (cons (append `(when* ,(car tree))
                     (list (cons 'begin-true (transform-levels-helper (cdadr tree)))))
                     ;`((begin ,(transform-levels-helper (cdadr tree)))))
             (transform-levels-helper (cddr tree)))]
      [else 
       (cons (car tree)
             (transform-levels-helper (cdr tree)))]))
  
  ;; remove one level of list
  (car (transform-levels-helper tree)))


;; probably don't need these
(define (make-expr test)
  (list test))

(define (make-test func expr)
  (cons func expr))

(define (get-func test)
  (car test))

(define (get-expr test)
  (cdr test))

;; not final. doesn't work if level drops. need a new algorithm.
;; probably not going to use
(define (build-exp lines last-line cur-level new-level)
  (cond 
    [(null? lines) (make-expr (make-test (car last-line) '()))]
    [(and (pair? (car lines)) 
          (equal? (caar lines) 'level))
     (printf "increment level~n") 
     (build-exp (cdr lines) last-line cur-level (add1 new-level))]
    [(= new-level (add1 cur-level))
     (printf "add new line with higher level~n") 
     (if (null? last-line)
         (build-exp (cdr lines) 
                    (car lines) 
                    (add1 cur-level) 
                    0)
         (make-expr (make-test (car last-line) 
                               (make-expr 
                                (build-exp (cdr lines) 
                                           (car lines) 
                                           (add1 cur-level) 
                                           0)))))]
    [(= new-level cur-level)
     (printf "new line at same level~n") 
     (if (null? last-line)
         (build-exp (cdr lines) 
                    (car lines)
                    cur-level 
                    0)
         (cons (make-test (car last-line) '()) 
               (build-exp (cdr lines) (car lines) cur-level 0)))]
    ;[(< new-level cur-level)
     
    ))


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
