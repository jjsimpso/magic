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

;; I don't think the null terminated read works with magic. The null bytes must be made explicit in the comparison.
(define (read-string8 [len 0])
  (if (> len 0)
      (bytes->string/utf-8 (read-bytes len))
      (bytes->string/utf-8 (read-null-terminated-ascii-string))))

(define (read-leshort)
  (let ([data (read-bytes 2)])
    (bitwise-ior (arithmetic-shift (bytes-ref data 1) 8)
                 (bytes-ref data 0))))

(define (read-lelong)
  (let ([data (read-bytes 4)])
    (bitwise-ior (arithmetic-shift (bytes-ref data 3) 24)
                 (arithmetic-shift (bytes-ref data 2) 16)
                 (arithmetic-shift (bytes-ref data 1) 8)
                 (bytes-ref data 0))))

(define (offset off)
  off)

(define (indoff initial-offset [read-func read-lelong])
  (file-position (current-input-port) initial-offset)
  (let ([off (read-func)])
    off))

(define (message text)
  text)

;; returns a function to read indirect offset from file
(define (size size-expr)
  (case size-expr
    [((lelong ".l")) read-lelong]
  ))

;; returns a function to read the needed data from the file
;; some types of comparisons benefit from knowing the data to compare to when reading from the file
;; for string8s, for instance, we need to read the exact number of bytes required to match the string
(define (type type-expr compare-expr)
  (case type-expr
    ;[((string8 "string")) read-string8]
    [((string8 "string")) 
     (let ([len (string-length (cadr compare-expr))]) ; len calculation may not work for all cases
       (lambda () (bytes->string/utf-8 (read-bytes len))))]
    ;[((search "string")) read-str]
    [((numeric "leshort")) read-leshort]))

;; returns a function to check the value read from the file
(define (compare compare-expr)
  (match compare-expr
    [(list 'strtest x) (lambda (s) (string=? s x))]
    [(list 'numtest "<" x) (lambda (n) (< n x))]
    [(list 'numtest ">" x) (lambda (n) (> n x))]
    [_ 'nothing]))

;; ex: (with-input-from-file "adventure.rkt" (lambda () (magic-test 0 (type '(string8 "string") '(strtest "MZ")) (compare '(strtest "MZ")) "dos executable")))
;; ex: (with-input-from-file "/tmp/iexplore.exe" (lambda () (magic-test (indoff 60 (size '(lelong ".l"))) (type '(string8 "string") '(strtest "PE\u0000\u0000")) (compare '(strtest "PE\u0000\u0000")) "PE executable (MS-Windows)")))
(define (magic-test off read-func compare-func message)
  (file-position (current-input-port) off)
  (let* ([data (read-func)]
         [result (compare-func data)])
    (when result (printf "~a~n" message))
    result))
