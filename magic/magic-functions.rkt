#lang racket

(define-struct test-type
  ([type]
   [flags])
  #:transparent)

(define (read-null-terminated-ascii-string [port (current-input-port)])
   (let loop ((result #""))
      (let ((next-char (read-byte port)))
         (if (or (eof-object? next-char) (= next-char 0) (> next-char 127))
             result
             (loop (bytes-append result (bytes next-char)))))))

(define (read-string8 [len 0])
  (if (> len 0)
      (bytes->string/utf-8 (read-bytes len))
      (bytes->string/utf-8 (read-null-terminated-ascii-string))))

(define (read-leshort)
  (read-bytes 2))

(define (offset off)
  off)

;; returns a function to read the needed data from the file
(define (type type-expr)
  (case type-expr
    [((string8 "string")) read-string8]
    ;[((search "string")) read-str]
    [((numeric "leshort")) read-leshort]))

;; returns a function to check the value read from the file
(define (compare compare-expr)
  (match compare-expr
    [(list 'strtest x) (lambda (s) (string=? s x))]
    [(list 'numtest "<" x) (lambda (n) (< n x))]
    [_ 'nothing]))

;; ex: (with-input-from-file "adventure.rkt" (lambda () (magic-test 0 (type '(string8 "string")) (compare '(strtest "MZ")) "dos executable")))
(define (magic-test off read-func compare-func message)
  (file-position (current-input-port) off)
  (let* ([data (read-func)]
         [result (compare-func data)])
    (when result (printf "~a~n" message))
    result))
