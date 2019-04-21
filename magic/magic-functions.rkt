#lang racket

(provide magic-test)
;(provide offset)
(provide type)
(provide compare)
(provide indoff)
(provide read-byt read-short read-long read-quad read-float read-double)
(provide read-leshort read-lelong read-lequad read-lefloat read-ledouble)
(provide read-beshort read-belong read-bequad read-befloat read-bedouble)
(provide read-byt-signed read-short-signed read-long-signed read-quad-signed)
(provide read-leshort-signed read-lelong-signed read-lequad-signed)
(provide read-beshort-signed read-belong-signed read-bequad-signed)

(define (read-null-terminated-ascii-string [port (current-input-port)])
   (let loop ((result #""))
      (let ((next-char (read-byte port)))
         (if (or (eof-object? next-char) (= next-char 0) (> next-char 127))
             result
             (loop (bytes-append result (bytes next-char)))))))

(define (read-byt [signed? #f])
  (let ([data (read-byte)])
    (when (eof-object? data) (error "eof"))
    (integer-bytes->integer (bytes data) signed?)))

(define (read-short [signed? #f])
  (let ([data (read-bytes 2)])
    (when (eof-object? data) (error "eof"))
    (when (< (bytes-length data) 2) (error "failure to read sufficient data"))
    (integer-bytes->integer data signed? (system-big-endian?))))

(define (read-long [signed? #f])
  (let ([data (read-bytes 4)])
    (when (eof-object? data) (error "eof"))
    (when (< (bytes-length data) 4) (error "failure to read sufficient data"))
    (integer-bytes->integer data signed? (system-big-endian?))))

(define (read-quad [signed? #f])
  (let ([data (read-bytes 8)])
    (when (eof-object? data) (error "eof"))
    (when (< (bytes-length data) 8) (error "failure to read sufficient data"))
    (integer-bytes->integer data signed? (system-big-endian?))))

(define (read-float)
  (let ([data (read-bytes 4)])
    (when (eof-object? data) (error "eof"))
    (when (< (bytes-length data) 4) (error "failure to read sufficient data"))
    (floating-point-bytes->real data (system-big-endian?))))

(define (read-double)
  (let ([data (read-bytes 8)])
    (when (eof-object? data) (error "eof"))
    (when (< (bytes-length data) 8) (error "failure to read sufficient data"))
    (floating-point-bytes->real data (system-big-endian?))))

;; I don't think the null terminated read works with magic. The null bytes must be made explicit in the comparison.
(define (read-string8 [len 0])
  (if (> len 0)
      (let ([data (read-bytes len)])
        (when (eof-object? data) (error "eof"))
        (bytes->string/utf-8 data))
      (bytes->string/utf-8 (read-null-terminated-ascii-string))))

(define (read-leshort [signed? #f])
  (let ([data (read-bytes 2)])
    (when (eof-object? data) (error "eof"))
    (when (< (bytes-length data) 2) (error "failure to read sufficient data"))
    (integer-bytes->integer data signed? #f)))

(define (read-lelong [signed? #f])
  (let ([data (read-bytes 4)])
    (when (eof-object? data) (error "eof"))
    (when (< (bytes-length data) 4) (error "failure to read sufficient data"))
    (integer-bytes->integer data signed? #f)))

(define (read-lequad [signed? #f])
  (let ([data (read-bytes 8)])
    (when (eof-object? data) (error "eof"))
    (when (< (bytes-length data) 8) (error "failure to read sufficient data"))
    (integer-bytes->integer data signed? #f)))

(define (read-lefloat)
  (let ([data (read-bytes 4)])
    (when (eof-object? data) (error "eof"))
    (when (< (bytes-length data) 4) (error "failure to read sufficient data"))
    (floating-point-bytes->real data #f)))

(define (read-ledouble)
  (let ([data (read-bytes 8)])
    (when (eof-object? data) (error "eof"))
    (when (< (bytes-length data) 8) (error "failure to read sufficient data"))
    (floating-point-bytes->real data #f)))

(define (read-beshort [signed? #f])
  (let ([data (read-bytes 2)])
    (when (eof-object? data) (error "eof"))
    (when (< (bytes-length data) 2) (error "failure to read sufficient data"))
    (integer-bytes->integer data signed? #t)))

(define (read-belong [signed? #f])
  (let ([data (read-bytes 4)])
    (when (eof-object? data) (error "eof"))
    (when (< (bytes-length data) 4) (error "failure to read sufficient data"))
    (integer-bytes->integer data signed? #t)))

(define (read-bequad [signed? #f])
  (let ([data (read-bytes 8)])
    (when (eof-object? data) (error "eof"))
    (when (< (bytes-length data) 8) (error "failure to read sufficient data"))
    (integer-bytes->integer data signed? #t)))

(define (read-befloat)
  (let ([data (read-bytes 4)])
    (when (eof-object? data) (error "eof"))
    (when (< (bytes-length data) 4) (error "failure to read sufficient data"))
    (floating-point-bytes->real data #t)))

(define (read-bedouble)
  (let ([data (read-bytes 8)])
    (when (eof-object? data) (error "eof"))
    (when (< (bytes-length data) 8) (error "failure to read sufficient data"))
    (floating-point-bytes->real data #t)))

(define (read-byt-signed)
  (read-byt #t))

(define (read-short-signed)
  (read-short #t))

(define (read-long-signed)
  (read-long #t))

(define (read-quad-signed)
  (read-quad #t))

(define (read-leshort-signed)
  (read-leshort #t))

(define (read-lelong-signed)
  (read-lelong #t))

(define (read-lequad-signed)
  (read-lequad #t))

(define (read-beshort-signed)
  (read-beshort #t))

(define (read-belong-signed)
  (read-belong #t))

(define (read-bequad-signed)
  (read-bequad #t))

(define (offset off)
  off)

(define (indoff initial-offset [read-func read-lelong] [operation #f] [arg #f])
  (with-handlers ([exn:fail? (lambda (exn) #f)])
    (file-position (current-input-port) initial-offset)
    (let ([off (read-func)])
      (if (and operation arg)
          (operation off arg)
          off))))

(define (message text)
  text)

;; returns a function to read indirect offset from file
(define (size size-expr)
  (case size-expr
    [((leshort ".s")) read-leshort]
    [((lelong ".l")) read-lelong]
    [((beshort ".S")) read-beshort]
    [((belong ".L")) read-belong]
  ))

(define (op op-expr)
  (case op-expr
    [("+") +]
    [("-") -]
    [("*") *]
    [("/") /]
    [("%") remainder]
    [("&") bitwise-and]
    [("|") bitwise-ior]
    [("^") bitwise-xor]))

(define (disp arg)
  arg)

;; returns a function to read the needed data from the file
;; some types of comparisons benefit from knowing the data to compare to when reading from the file
;; for string8s, for instance, we need to read the exact number of bytes required to match the string
(define (type type-expr compare-expr)
  (define signed-compare (if (and (eq? (car compare-expr) 'numtest)
                                  (< (last compare-expr) 0))
                             #t
                             #f))
  ;(printf "type expr: ") (display type-expr) (printf "~n")
  (case type-expr
    ;[((string8 "string")) read-string8]
    [((string8 "string")) 
     (let ([len (string-length (last compare-expr))]) ; len calculation may not work for all cases
                                ; the last here is making the assumption that the string to search for
                                ; is the last element of the (strtest ...) form
       (lambda () 
         (let ([data (read-bytes len)])
           (if (eof-object? data)
               (error "eof")
               (bytes->string/latin-1 data)))))]
    ;[((search "string")) read-str]
    [((numeric "byte")) (if signed-compare read-byt-signed read-byt)]
    [((numeric "u" "byte")) read-byt]
    [((numeric "short")) (if signed-compare read-short-signed read-short)]
    [((numeric "u" "short")) read-short]
    [((numeric "long")) (if signed-compare read-long-signed read-long)]
    [((numeric "u" "long")) read-long]
    [((numeric "quad")) (if signed-compare read-quad-signed read-quad)]
    [((numeric "u" "quad")) read-quad]
    [((numeric "float")) read-float]
    [((numeric "double")) read-double]
    [((numeric "leshort")) (if signed-compare read-leshort-signed read-leshort)]
    [((numeric "u" "leshort")) read-leshort]
    [((numeric "lelong")) (if signed-compare read-lelong-signed read-lelong)]
    [((numeric "u" "lelong")) read-lelong]
    [((numeric "lequad")) (if signed-compare read-lequad-signed read-lequad)]
    [((numeric "u" "lequad")) read-lequad]
    [((numeric "lefloat")) read-lefloat]
    [((numeric "ledouble")) read-ledouble]
    [((numeric "beshort")) (if signed-compare read-beshort-signed read-beshort)]
    [((numeric "u" "beshort")) read-beshort]
    [((numeric "belong")) (if signed-compare read-belong-signed read-belong)]
    [((numeric "u" "belong")) read-belong]
    [((numeric "bequad")) (if signed-compare read-bequad-signed read-bequad)]
    [((numeric "u" "bequad")) read-bequad]
    [((numeric "befloat")) read-befloat]
    [((numeric "bedouble")) read-bedouble]
    ))

;; returns a function to check the value read from the file
(define (compare compare-expr)
  (match compare-expr
    [(list 'strtest x) (lambda (s) (string=? s x))]
    [(list 'strtest "<" x) (lambda (s) (string<? s x))]
    [(list 'strtest ">" x) (lambda (s) (string>? s x))]
    [(list 'strtest "=" x) (lambda (s) (string=? s x))]
    [(list 'numtest x) (lambda (n) (= n x))]
    [(list 'numtest "<" x) (lambda (n) (< n x))]
    [(list 'numtest ">" x) (lambda (n) (> n x))]
    [(list 'numtest "!" x) (lambda (n) (not (= n x)))]
    ; the next three haven't been fully tested
    [(list 'numtest "&" x) (lambda (n) 
                             (not (= (bitwise-and n x) 
                                     0)))]
    [(list 'numtest "^" x) (lambda (n)
                             (not
                              (bitwise-ior n (bitwise-and n x))))]
    [(list 'numtest "~" x) (lambda (n) (= n (bitwise-not x)))]
    [(list 'numtest "=" x) (lambda (n) (= n x))]
    [_ 'nothing]))

;; ex: (with-input-from-file "adventure.rkt" (lambda () (magic-test 0 (type '(string8 "string") '(strtest "MZ")) (compare '(strtest "MZ")) "dos executable")))
;; ex: (with-input-from-file "/tmp/iexplore.exe" (lambda () (magic-test (indoff 60 (size '(lelong ".l"))) (type '(string8 "string") '(strtest "PE\u0000\u0000")) (compare '(strtest "PE\u0000\u0000")) "PE executable (MS-Windows)")))
(define (magic-test off read-func compare-func [message ""])
  (with-handlers ([exn:fail? (lambda (exn) 
                               (printf "error: ~a~n" (exn-message exn))
                               #f)])
    ;(printf "running magic-test: ~a,~a,~a~n" off read-func compare-func)
    ;(display off)
    ;(display read-func)
    ;(display compare-func)
    (file-position (current-input-port) off)
    (let* ([data (read-func)]
           [result (compare-func data)])
      (when (and result (non-empty-string? message))
        (printf "~a" message))
      result)))
