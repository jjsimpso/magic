#lang racket

(define-struct test-type
  ([type]
   [flags])
  #:transparent)


(define (read-str)
  "MZ")

(define (read-leshort)
  (read-bytes 2))

(define (offset off)
  off)

;; returns a function to read the needed data from the file
(define (type type-expr)
  (case type-expr
    [((search "string")) read-str]
    [((numeric "leshort")) read-leshort]))

;; returns a function to check the value read from the file
(define (compare compare-expr)
  (match compare-expr
    [(list 'strtest x) (lambda (s) (string=? s x))]
    [(list 'numtest "<" x) (lambda (n) (< n x))]
    [_ 'nothing]))

(define (magic-test off read-func compare-func message)
  (file-position (current-input-port) off)
  (let* ([data (read-func)]
         [result (compare-func data)])
    (when result (printf "~a~n" message))
    result))
