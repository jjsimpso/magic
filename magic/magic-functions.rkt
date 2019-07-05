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
(define (read-string8 len)
  (let ([data (read-bytes len)])
    (if (eof-object? data)
        (error "eof")
        (bytes->string/latin-1 data))))

(define (whitespace? b)
  (if (eof-object? b)
      #f
      (or (= b 32)
          (and (>= b 9) (<= b 13)))))

(define (discard-whitespace)
  (when (whitespace? (peek-byte))
    (read-byte)
    (discard-whitespace)))

;; don't know correct behavior, stub out for now
(define (read-string8-trim-ws len)
  (read-string8 len))

(define (read-string8-compact-ws len)
  (let loop ([num-read 0]
             [data #""])
    (if (= num-read len) 
        (bytes->string/latin-1 data)
        (let ([next-char (read-byte)])
          (cond [(eof-object? next-char) (bytes->string/latin-1 data)]
                [(whitespace? next-char)
                 (discard-whitespace)
                 (loop (add1 num-read)
                       (bytes-append data (bytes next-char)))]
                [else 
                 (loop (add1 num-read) 
                       (bytes-append data (bytes next-char)))])))))

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
  (match type-expr
    [(list 'string8 "string" (list 'strflag flag) ...)
     (let ([len (string-length (last compare-expr))]
           [binary? (member "b" flag)]
           [text? (member "t" flag)]
           [trim? (member "T" flag)]
           [compact-whitespace? (member "W" flag)])
       (cond
         ;[(and trim? compact-whitespace?)]
         [trim? (lambda () (read-string8-trim-ws len))]
         [compact-whitespace? (lambda () (read-string8-compact-ws len))]
         [else (lambda () (read-string8 len))]))]
    [(list 'string8 "string") 
     (let ([len (string-length (last compare-expr))]) ; len calculation may not work for all cases
                                ; the last here is making the assumption that the string to search for
                                ; is the last element of the (strtest ...) form
       (lambda () 
         (read-string8 len)))]
    [(list 'search "string" (list 'srchflag flag) ...)
     (let ([len (string-length (last compare-expr))]
           [binary? (member "b" flag)]
           [text? (member "t" flag)]
           [trim? (member "T" flag)]
           [compact-whitespace? (member "W" flag)])
       (lambda () 
         (let ([data (read-bytes len)])
           (when (eof-object? data) (error "eof"))
           (define str (bytes->string/latin-1 data))
           (when trim? (set! str (string-trim str)))
           (when compact-whitespace? (set! str (string-normalize-spaces str #:trim? #f)))
           str)))]
    [(list 'numeric "byte") (if signed-compare read-byt-signed read-byt)]
    [(list 'numeric "u" "byte") read-byt]
    [(list 'numeric "short") (if signed-compare read-short-signed read-short)]
    [(list 'numeric "u" "short") read-short]
    [(list 'numeric "long") (if signed-compare read-long-signed read-long)]
    [(list 'numeric "u" "long") read-long]
    [(list 'numeric "quad") (if signed-compare read-quad-signed read-quad)]
    [(list 'numeric "u" "quad") read-quad]
    [(list 'numeric "float") read-float]
    [(list 'numeric "double") read-double]
    [(list 'numeric "leshort") (if signed-compare read-leshort-signed read-leshort)]
    [(list 'numeric "u" "leshort") read-leshort]
    [(list 'numeric "lelong") (if signed-compare read-lelong-signed read-lelong)]
    [(list 'numeric "u" "lelong") read-lelong]
    [(list 'numeric "lequad") (if signed-compare read-lequad-signed read-lequad)]
    [(list 'numeric "u" "lequad") read-lequad]
    [(list 'numeric "lefloat") read-lefloat]
    [(list 'numeric "ledouble") read-ledouble]
    [(list 'numeric "beshort") (if signed-compare read-beshort-signed read-beshort)]
    [(list 'numeric "u" "beshort") read-beshort]
    [(list 'numeric "belong") (if signed-compare read-belong-signed read-belong)]
    [(list 'numeric "u" "belong") read-belong]
    [(list 'numeric "bequad") (if signed-compare read-bequad-signed read-bequad)]
    [(list 'numeric "u" "bequad") read-bequad]
    [(list 'numeric "befloat") read-befloat]
    [(list 'numeric "bedouble") read-bedouble]
    [(list 'default "default") (lambda () 42)] ; could return anything
    [_ (error (string-append "type expression doesn't match: " (~a type-expr)))]
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
    [(list 'truetest "x") (lambda (n) #t)]
    [_ (error (string-append "test expression doesn't match: " (~a compare-expr)))]))

(define (single-cprintf-sub str val)
  (if (string-contains? str "%d") 
      (format (string-replace str "%d" "~a" #:all? #f) val)
      str))

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
        (printf "~a " (single-cprintf-sub message data)))
      result)))
