#lang racket

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

(provide parse-levels)
(provide transform-levels)
(provide parse-level0)

(require syntax/parse)
(require (for-syntax syntax/parse))

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

(begin-for-syntax 
  (define-syntax-class mag-lvl
    (pattern ({~datum level})))
  
  (define-syntax-class mag-line
    (pattern ({~literal line} expr ...)))
)

;; temporary
(define-syntax (line stx)
  (syntax-parse stx
    [(_ e ...) #'(quote (e ...))]))

#;(define-syntax (splice-to-level stx)
  (syntax-parse stx
    [(_ ln1:mag-line)
     #'ln1]
    [(_ ln1:mag-line ln2:mag-line expr ...)
     #'(ln1 (splice-to-level ln2 expr ...))]
    ))

(define-for-syntax (splice-to-level stx)
  (display stx)
  (syntax-parse stx
    [(ln1:mag-line)
     (printf "splice to level 1~n")
     #'(ln1)]
    [(ln1:mag-line ln2:mag-line expr ...)
     (printf "splice to level 2~n")
     #`(ln1 #,@(splice-to-level #'(ln2 expr ...)))]
    [(ln:mag-line ({~literal level} lexpr ...) expr ...)
     (printf "splice to level 3~n")
     #`((when ln (level lexpr ...))
        #,@(splice-to-level #'(expr ...)))]
    [() #'()]))

(define-syntax (level stx)
  (syntax-parse stx
    [(_ ln:mag-line ...)
     #'(begin ln ...)]
    [(_ ln:mag-line ({~literal level} lexpr ...) . rst)
     (with-syntax ([(expr ...) #'rst])
       #`(begin 
           (when ln (level lexpr ...))
           #,@(splice-to-level #'rst)))]
    [(_ ln:mag-line . rst)
     #`(begin 
         ln
         #,@(splice-to-level #'rst))]))

;; (syntax-parse #'((level) (level)) [(((~datum level)) ((~datum level))) #t])
;; (syntax-parse #'(line 0 run-test) [(line e ...) #t])
;; (syntax-parse #'(line 0 run-test) [({~literal line} expr ...) #'(line expr ...)])
;; (syntax-parse #'((level) (line 0 run-test) (line 1 run-test)) [(({~datum level}) ...+ ({~literal line} expr ...) ...) #'(level (line expr ...) ...)])
;; (syntax-parse #'((level) (line 0 run-test) (line 1 run-test) (level)) [(lvl:mag-lvl ...+ ln:mag-line ~rest r) #'r])
;; (parse-level0 (line (offset 0) (type (default "default")) (test (truetest "x"))))
;; (parse-level0 (level) (line (offset 0) (type (default "default")) (test (truetest "x"))))
(define-syntax (parse-level0 stx)
  (syntax-parse stx
    ;[(_ ln:mag-line) #'(ln)]
    [(_ ln:mag-line . rst) #'(ln (parse-level1 rst))]
    [(_ lvl:mag-lvl ln:mag-line) 
     #'(level ln)]
    [_ #'()]))

(define-syntax (parse-level1 stx)
  (syntax-parse stx
    [(_ ()) #'void]
    [_ #f]))

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
