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
(provide last-level-offset any-true? begin-level test-passed-at-level when*)
(provide (for-syntax reverse-endianness) level parse-level0)

(require syntax/parse racket/stxparam)
(require (for-syntax syntax/stx syntax/parse racket/syntax racket/list))

(define-syntax-parameter last-level-offset
  (lambda (stx)
    (raise-syntax-error (syntax-e stx) "can only be used inside any-true? or begin-level")))

(define test-passed-at-level (make-parameter #f))

;; run all top level tests and ...
;; initialize last-level-offset to 0
(define-syntax-rule (filter-highest-strength body ...)
  (let ([result #f]
        [tmp-offset 0])
    (syntax-parameterize ([last-level-offset (make-rename-transformer #'tmp-offset)])
      ;(printf "any-true: last level offset is ~a~n" last-level-offset)
      (when body (set! result #t))
      ...
      result)))

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

;; like any-true? but always returns true
(define-syntax-rule (begin-level body ...)
  (let ([tmp-offset (file-position (current-input-port))])
    (syntax-parameterize ([last-level-offset (make-rename-transformer #'tmp-offset)])
      (parameterize ([test-passed-at-level #f])
        body
        ...)
      #t)))

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
    (pattern ((~or* {~datum line} {~datum clear-line}) expr ...)))

  (define-syntax-class lvl>0
    #:datum-literals (level)
    #:literals (line)
    ;(pattern ((~between ({~datum level}) 2 +inf.0) ln:mag-line)))
    ;(pattern (lvl:mag-level lvlx:mag-level ...+ ln:mag-line)))
    (pattern (lvl:mag-lvl ...+ ln:mag-line)))

  (define-syntax-class endian-numeric
    (pattern (~or "leshort" "lelong" "lequad" "lefloat" "ledouble" "beshort" "belong" "bequad" "befloat" "bedouble")))
)

;; TODO: catch error when val is greater than size of int as indicated by blen
;; (would be nice to get a syntax error in this case)
(define-for-syntax (reverse-bytes val blen signed? big-endian?)
  (if (inexact? val)
      (floating-point-bytes->real
       ; only support signed floats
       (real->floating-point-bytes val blen #t (not big-endian?))
       #t
       big-endian?)
      (integer-bytes->integer 
       (integer->integer-bytes val blen signed? (not big-endian?))
       signed?
       big-endian?)))

(define-for-syntax (reverse-test-value-endianness blen big-endian? stx)
  (syntax-parse stx #:datum-literals (test numtest)
    [(test (numtest op:string val)) 
     #:when (or (string=? (syntax-e #'op) ">") (string=? (syntax-e #'op) "<"))
     ; should we always treat as signed or treat as unsigned unless val is a negative number?
     #`(test (numtest op #,(reverse-bytes (syntax->datum #'val) blen (< (syntax->datum #'val) 0) big-endian?)))]
    [(test (numtest (~optional op:string) val)) 
     ; treat as unsigned unless val is a negative number
     #`(test (numtest (~? op) #,(reverse-bytes (syntax->datum #'val) blen (< (syntax->datum #'val) 0) big-endian?)))]
    [(expr ...)
     #'(expr ...)]))

(define-for-syntax (reverse-mask-value-endianness blen stx)
  (syntax-parse stx #:datum-literals (nummask op)
    [(nummask (op operator:string) val)
     #`(nummask (op operator) #,(reverse-bytes (syntax->datum #'val) blen #f #f))]
    [#f #'()]
    [(expr ...)
     #'(expr ...)]))

;; TODO: add support for all type parameters
(define-for-syntax (reverse-line-endianness stx)
  (syntax-parse stx
    ;; little endian
    [(ln off-expr (type (numeric (~optional u:string) "leshort")) test-expr ~rest msg-expr)
     #`(ln off-expr (type (numeric (~? u) "beshort")) test-expr . msg-expr)]
    [(ln off-expr (type (numeric (~optional u:string) "lelong")) test-expr ~rest msg-expr)
     #`(ln off-expr (type (numeric (~? u) "belong")) test-expr . msg-expr)]
    [(ln off-expr (type (numeric (~optional u:string) "lequad")) test-expr ~rest msg-expr)
     #`(ln off-expr (type (numeric (~? u) "bequad")) test-expr . msg-expr)]
    [(ln off-expr (type (numeric (~optional u:string) "lefloat")) test-expr ~rest msg-expr)
     #`(ln off-expr (type (numeric (~? u) "befloat")) test-expr . msg-expr)]
    [(ln off-expr (type (numeric (~optional u:string) "ledouble")) test-expr ~rest msg-expr)
     #`(ln off-expr (type (numeric (~? u) "bedouble")) test-expr . msg-expr)]
    ;; big endian
    [(ln off-expr (type (numeric (~optional u:string) "beshort")) test-expr ~rest msg-expr)
     #`(ln off-expr (type (numeric (~? u) "leshort")) test-expr . msg-expr)]
    [(ln off-expr (type (numeric (~optional u:string) "belong")) test-expr ~rest msg-expr)
     #`(ln off-expr (type (numeric (~? u) "lelong")) test-expr . msg-expr)]
    [(ln off-expr (type (numeric (~optional u:string) "bequad")) test-expr ~rest msg-expr)
     #`(ln off-expr (type (numeric (~? u) "lequad")) test-expr . msg-expr)]
    [(ln off-expr (type (numeric (~optional u:string) "befloat")) test-expr ~rest msg-expr)
     #`(ln off-expr (type (numeric (~? u) "lefloat")) test-expr . msg-expr)]
    [(ln off-expr (type (numeric (~optional u:string) "bedouble")) test-expr ~rest msg-expr)
     #`(ln off-expr (type (numeric (~? u) "ledouble")) test-expr . msg-expr)]
    ;; Reverse endianness of used names
    [(ln off-expr (type (use "use")) (test (use-name magic-name)))
     (with-syntax ([^magic-name (format-id stx "^~a" (syntax-e #'magic-name))])
       #`(ln off-expr (type (use "use")) (test (use-name ^magic-name))))]
    ;; catch everything else
    [(expr ...)
     ;(printf "no endianness conversion~n")
     #'(expr ...)]))

(define-for-syntax (reverse-endianness stx)
  ;(printf "reverse-endiannes: ") (display stx) (printf "~n")
  (syntax-parse stx
    [(lvl:mag-lvl ...+ expr ...+) 
     ;(printf "match 1~n")
     #`(lvl ... #,@(reverse-endianness #'(expr ...)))]
    [(ln:mag-line expr ...+)
     ;(printf "match 2~n")
     #`(#,(reverse-line-endianness #'ln) #,@(reverse-endianness #'(expr ...)))]
    [(ln:mag-line)
     ;(printf "match 3~n")
     #`(#,(reverse-line-endianness #'ln))]))

;; temporary
#;(define-syntax (line stx)
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
     #`((when* ln (level lexpr ...))
        #,@(splice-to-level #'(expr ...)))]
    [() #'()]))

(define-syntax (level stx)
  (syntax-parse stx
    [(_ ln:mag-line ...)
     #'(begin-level ln ...)]
    [(_ ln:mag-line ({~literal level} lexpr ...) . rst)
     (with-syntax ([(expr ...) #'rst])
       #`(begin-level 
           (when* ln (level lexpr ...))
           #,@(splice-to-level #'rst)))]
    [(_ ln:mag-line . rst)
     #`(begin-level 
         ln
         #,@(splice-to-level #'rst))]))

;; (syntax-parse #'((level) (level)) [(((~datum level)) ((~datum level))) #t])
;; (syntax-parse #'(line 0 run-test) [(line e ...) #t])
;; (syntax-parse #'(line 0 run-test) [({~literal line} expr ...) #'(line expr ...)])
;; (syntax-parse #'((level) (line 0 run-test) (line 1 run-test)) [(({~datum level}) ...+ ({~literal line} expr ...) ...) #'(level (line expr ...) ...)])
;; (syntax-parse #'((level) (line 0 run-test) (line 1 run-test) (level)) [(lvl:mag-lvl ...+ ln:mag-line ~rest r) #'r])
;; (parse-level0 (line (offset 0) (type (default "default")) (test (truetest "x"))))
;; (parse-level0 (level) (line (offset 0) (type (default "default")) (test (truetest "x"))))
;; (syntax-parse #'(1 2 2 a 2 2 b 2 c) [(1 (~seq n:nat ...+ x) ...) #'((~@ n ... x) ...)])
;; (syntax-parse #'(1 2 3) [((~between x:integer 3 3) ...) #'1])
;; (syntax-parse #'(1 2 3 'foo) [((~seq (~between x:integer 2 2) ... y:integer ...+) expr) #'foo])
(define-syntax (parse-level0 stx)
  ;(printf "parse-level0 input: ")
  ;(display stx) (printf "~n")
  (syntax-parse stx
    [(_ ln:mag-line) #'ln]
    ;[(_ ln:mag-line lvl:mag-lvl ln2:mag-line expr ...)
    [(_ ln:mag-line (~seq lvl:mag-lvl ...+ ln2:mag-line) ...+)
     #:with branch-lines #'((~@ lvl ... ln2) ...)
     ;(printf "parse-level0 1~n")
     #`(when* ln (level 
            #,@(parse-level1 #'branch-lines)))]
    [(_ lvl:mag-lvl ...+ expr ...) 
     #'(error "no line at level 0")]
    [_
     ;(printf "parse-level0 error on input: ")
     ;(display stx) (printf "~n") 
     #'(error "syntax error at level 0")]))

(define-for-syntax (parse-level1 stx)
  ;(printf "parse-level1 input: ")
  ;(display stx) (printf "~n")
  (syntax-parse stx
    [(lvl:mag-lvl ln:mag-line expr ...)
     ;(printf "parse-level1 1~n")
     #`(ln #,@(parse-level1 #'(expr ...)))]
    [(lvlx:mag-lvl lvly:mag-lvl ln:mag-line (~seq lvl1:mag-lvl lvl2:mag-lvl ...+ ln2:mag-line) ... expr ...)
     #:with branch-lines #'((~@ lvl1 lvl2 ... ln2) ...)
     ;(printf "parse-level1 2~n")
     #`((level ln #,@(parse-level2 #'branch-lines))
        #,@(parse-level1 #'(expr ...)))]
    [()
     ;(printf "parse-level1 3~n") 
     #'()]))

(define-syntax (define-parse-level-func stx)
  (display stx)
  (syntax-parse stx
    [(_ plevel:integer)
     #:with name (format-id stx "parse-level~a" (syntax-e #'plevel))
     #:with nested-name (format-id stx "parse-level~a" (+ 1 (syntax-e #'plevel)))
     #:with nlevel (datum->syntax #'plevel (+ 1 (syntax-e #'plevel)))
     #:with level-prefix (datum->syntax #'plevel (make-list (syntax-e #'plevel) 'lvl))
     #:with ooo (quote-syntax ...)
     #:with ooo+ (quote-syntax ...+)
     #'(define-for-syntax (name stx)
         ;(printf "~a input: " name)
         ;(display stx) (printf "~n")
         (syntax-parse stx
           [((~between lvl:mag-lvl plevel plevel) ooo ln:mag-line expr ooo)
            ;(printf "~a 1~n" name)
            #`(ln #,@(name #'(expr ooo)))]
           [((~between lvlx:mag-lvl nlevel nlevel) ooo ln:mag-line 
             (~seq (~between lvl1:mag-lvl plevel plevel) ooo lvl2:mag-lvl ooo+ ln2:mag-line) ooo expr ooo)
            #:with branch-lines #'(((... ~@) lvl1 ooo lvl2 ooo ln2) ooo)
            ;(printf "~a 2: " name)
            ;(display #'branch-lines) (printf "~n")
            #`((level ln #,@(nested-name #'branch-lines))
               #,@(name #'(expr ooo)))]
           [()
            ;(printf "~a 3~n" name)
            #'()]
           [_
            (printf "~a error on input: " name)
            (display stx) (printf "~n")  
            #'(error "syntax error at level x")]))]
     [(_) (error "invalid argument")]))


(define-parse-level-func 2)
(define-parse-level-func 3)
(define-parse-level-func 4)
(define-parse-level-func 5)
(define-parse-level-func 6)
(define-parse-level-func 7)
(define-parse-level-func 8)
(define-parse-level-func 9)

;; stub out the deepest level possible to resolve unbound reference and stop the recursion
(define-for-syntax (parse-level10 stx)
  (printf "parse-level10 input: ")
  (display stx)
  (error "Exceeded max nesting level of 9")
  #'())

#;(define-for-syntax (parse-level2 stx)
  (printf "parse-level2 input: ")
  (display stx)
  (syntax-parse stx
    [()
     (printf "parse-level2 4~n") 
     #'()]))
    
  
;; transforms code from something like this:
;; ((line1)
;;  (level (line2)
;;         (line3)
;;         (level (line4)
;;                (line5))))
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
                     (list (cons 'begin-level (transform-levels-helper (cdadr tree)))))
                     ;`((begin ,(transform-levels-helper (cdadr tree)))))
             (transform-levels-helper (cddr tree)))]
      [else 
       (cons (car tree)
             (transform-levels-helper (cdr tree)))]))
  
  ;; remove one level of list
  (car (transform-levels-helper tree)))
