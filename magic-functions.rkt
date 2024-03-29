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

(provide magic-test)
(provide type)
(provide compare)
(provide indoff)
(provide offset-from-eof)
(provide read-byt read-short read-long read-quad read-float read-double)
(provide read-leshort read-lelong read-lequad read-lefloat read-ledouble)
(provide read-beshort read-belong read-bequad read-befloat read-bedouble)

(require racket/date)

(require "output.rkt")
         
;; only need the parameter test-passed-at-level
(require (only-in "expander-utils.rkt" test-passed-at-level))

;; doesn't include any leap seconds. are they needed?
(define windows-secs-to-epoch 11644473600)

;; can modify to change cprintf date formatting
;(date-display-format 'iso-8601)

(define (increment-file-position! amt)
  (file-position 
   (current-input-port)
   (+ (file-position (current-input-port)) amt)))

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

;; reads until a null byte is found or the max number of bytes are read
;; returns a string in latin-1 encoding
(define (read-cstring8 max-to-read)
  (define result (make-bytes max-to-read 0))
  (let loop ((num-read 0))
    (let ((next-char (read-byte)))
      (if (or (eof-object? next-char) (= next-char 0) (= num-read max-to-read))
          (bytes->string/latin-1 (subbytes result 0 num-read))
          (begin
            (bytes-set! result num-read next-char)
            (loop (add1 num-read)))))))

(define (read-string8 len)
  (let ([data (read-bytes len)])
    (if (eof-object? data)
        (error "eof")
        (bytes->string/latin-1 data))))

;; read a minimum of len bytes/characters from the current input port
;; if bytestring read doesn't end in a null byte, read until one is reached or a max number are read 
;; (read-cstring8 does this)
(define (read-string8-till-null len)
  (define data (read-bytes len))
  (when (eof-object? data) 
    (error (string-append "read-string8-till-null: eof, read len = " (~a len))))
  
  (define datalen (bytes-length data))
  (cond 
    [(< datalen len)
     (bytes->string/latin-1 data)]
    [(= (bytes-ref data (sub1 datalen)) 0)
     (bytes->string/latin-1 data)]
    [else
     (string-append (bytes->string/latin-1 data) (read-cstring8 255))]))

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

(define (pstring-flag-bytes flag)
  (cond
    [(or (string=? flag "H") (string=? flag "h")) 2]
    [(or (string=? flag "L") (string=? flag "l")) 4]
    [else 1]))

(define (pstring-flag-big-endian? flag)
  (cond
    [(or (string=? flag "H") (string=? flag "L")) #t]
    [else #f]))

(define (read-pstring len-bytes big-endian? jpeg?)
  (define rawlen (integer-bytes->integer (read-bytes len-bytes) #f big-endian?))
  (define len (if jpeg?
                  (- rawlen len-bytes)
                  rawlen))
  
  (let ([data (read-bytes len)])
    (if (eof-object? data)
        (error "eof")
        (bytes->string/latin-1 data))))

;; reads until a null byte is found or the max number of bytes are read
;; returns a byte string
(define (read-cstring16 max-to-read)
  (define result (make-bytes (* max-to-read 2) 0))
  (let loop ((num-read 0))
    (let ((next-char (read-bytes 2)))
      (if (or (eof-object? next-char)
              (bytes=? next-char #"\0\0")
              (= (bytes-length next-char) 1)
              (= num-read max-to-read))
          (subbytes result 0 (* num-read 2))
          (begin
            (bytes-copy! result (* num-read 2) next-char)
            (loop (add1 num-read)))))))

(define (read-string16 len [big-endian? #f])
  (define converter
    (if big-endian?
        (bytes-open-converter "UTF-16BE" "UTF-8")
        (bytes-open-converter "UTF-16LE" "UTF-8")))

  ;; discard the Byte Order Mark if it exists since we specifyi the endianness
  (define maybe-bom (peek-bytes 2 0))
  (when (or (equal? maybe-bom #"\377\376")
            (equal? maybe-bom #"\376\377"))
    (read-bytes 2))

  (define utf16-len (* len 2))
  
  (if converter
      (let ([data (read-bytes utf16-len)])
        (if (eof-object? data)
            (error (string-append "read-string16: eof, read len = " (~a len)))
            (let-values ([(conv-data num-bytes status) (bytes-convert converter data)])
              (define datalen (bytes-length conv-data))
              (cond 
                [(< datalen len)
                 (bytes->string/latin-1 conv-data)]
                [(= (bytes-ref conv-data (sub1 datalen)) 0)
                 (print-debug "string16 null string~n")
                 (bytes->string/latin-1 conv-data)]
                [else
                 (print-debug "reading cstring16~n")
                 (define-values (c16-data c16-num c16-status) (bytes-convert converter (read-cstring16 255)))
                 (string-append (bytes->string/latin-1 conv-data)
                                (bytes->string/latin-1 c16-data))]))))
      (error "UTF-16 conversion not supported!")))

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

;; return the file offset equal to 'off' bytes from the end of the file 
(define (offset-from-eof off)
  (file-position (current-input-port) eof)
  (define file-size (file-position (current-input-port)))
  (- file-size (abs off)))

;; the operation argument arg can be either an integer or a function to read a second indirect offset
;; to use as the arg (see disp macro in expander.rkt).
(define (indoff initial-offset [read-func read-lelong] [operation #f] [arg #f])
  (with-handlers ([exn:fail? (lambda (exn) #f)])
    (file-position (current-input-port) initial-offset)
    (print-debug "indoff: read offset from 0x~a = " (number->string initial-offset 16))
    (let ([off (read-func)])
      (print-debug "0x~a~n" (number->string off 16))
      (if (and operation arg)
          (if (procedure? arg)
              (operation off (arg read-func initial-offset))
              (operation off arg))
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

;; the search function should leave the file position set to its original value,
;; i.e. where the search began. then the compare function will update the 
;; file offset to either the beginning or end of the match, depending on the
;; flags.
;; cnt specifies the number of search attempts, not bytes. just assume a worst
;; case number of bytes to search.
(define (build-search-read-func cnt flags needle)
  (let ([len (* cnt (string-length needle))]
        [binary? (member "b" flags)]
        [text? (member "t" flags)]
        [trim? (member "T" flags)]
        [compact-whitespace? (member "W" flags)])
    ;(eprintf "build-search-read-func: flags = ~a~n" flags)
    (lambda () 
      (define start-offset (file-position (current-input-port)))
      (let ([data (read-bytes len)])
        (file-position (current-input-port) start-offset)
        ;(printf "search: setting offset to start of search ~a~n" start-offset)
        (when (eof-object? data) (error "eof"))
        (define str (bytes->string/latin-1 data))
        (when trim? (set! str (string-trim str)))
        (when compact-whitespace? (set! str (string-normalize-spaces str #:trim? #f)))
        str))))

(define (build-regex-read-func cnt flags)
  (define (read-lines cnt)
    (let loop ([i 0]
               [data #""])
      (cond
        [(eof-object? (peek-byte))
         data]
        [(< i cnt)
         (loop (add1 i) (bytes-append data (read-bytes-line) #"\n"))]
        [else
         data])))
  
  (if (member "l" flags)
      (lambda ()
        (define start-offset (file-position (current-input-port)))
        (let ([data (read-lines cnt)])
          (file-position (current-input-port) start-offset)
          (bytes->string/latin-1 data)))
      (lambda () 
        (define start-offset (file-position (current-input-port)))
        (let ([data (read-bytes cnt)])
          (file-position (current-input-port) start-offset)
          (when (eof-object? data) (error "eof"))
          (bytes->string/latin-1 data)))))

(define (build-numeric-read-func signed? mask-expr func)
  ; numeric read functions default to unsigned reads
  (if mask-expr
      (lambda () 
        (apply (op (car mask-expr))
               (list (second mask-expr) (func signed?))))
      (if signed?
          (lambda () (func #t))
          func)))
      
;; returns a function to read the needed data from the file
;; some types of comparisons benefit from knowing the data to compare to when reading from the file
;; for string8s, for instance, we need to read the exact number of bytes required to match the string
(define (type type-expr compare-expr)
  (define signed-compare (if (and (eq? (car compare-expr) 'numtest)
                                  (< (last compare-expr) 0))
                             #t
                             #f))

  (define mask-expr
    (match type-expr
      [(or (list 'numeric "u" _ (list 'nummask (list 'op op) x))
           (list 'numeric _ (list 'nummask (list 'op op) x)))
       (list op x)]
      [_ #f]))
  
  (define trimmed-type-expr
    (if mask-expr
        (match type-expr
          [(list x ... (list 'nummask (list 'op _) _)) x]
          [_ (error (string-append "type expression doesn't match for trimming nummask: " (~a type-expr)))])
        type-expr))
  
  ;(printf "type expr: ") (display type-expr) (printf "~n")
  (match trimmed-type-expr
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
         [else (lambda () (read-string8-till-null len))]))]
    [(list 'string8 "string")
     (let ([len (string-length (last compare-expr))])
       (lambda () (read-string8-till-null len)))]
    [(list 'pstring "pstring" (list 'pstrflag flag) (list 'pstrjflag jflag))
     (lambda ()
       (read-pstring (pstring-flag-bytes flag)
                     (pstring-flag-big-endian? flag)
                     #t))]
    [(list 'pstring "pstring" (list 'pstrflag flag))
     (lambda ()
       (read-pstring (pstring-flag-bytes flag)
                     (pstring-flag-big-endian? flag)
                     #f))]
    [(list 'pstring "pstring" (list 'pstrjflag jflag))
     (lambda ()
       (read-pstring 1 #t))]
    [(list 'pstring "pstring")
     (lambda ()
       (read-pstring 1 #f))]
    [(list 'search (list 'srchcnt cnt) (list 'strflag flag) ...)
     (build-search-read-func cnt flag (last compare-expr))]
    [(list 'search (list 'strflag flag) ... (list 'srchcnt cnt))
     (build-search-read-func cnt flag (last compare-expr))]
    [(list 'search (list 'strflag flag) ...)
     ; default count to 1024 for now, but need to read the entire file if necessary
     ; after the match is found, should the file offset be set to the end of the match?
     (build-search-read-func 1024 flag (last compare-expr))]
    [(list 'search)
     (build-search-read-func 1024 '() (last compare-expr))]
    [(list 'regex (list 'regcnt cnt) (list 'regflag flag) ...)
     (build-regex-read-func cnt flag)]
    [(list 'regex (list 'regflag flag) ... (list 'regcnt cnt))
     (build-regex-read-func cnt flag)]
    [(list 'regex (list 'regcnt cnt))
     (build-regex-read-func cnt '())]
    ; default count is 8KB for regex searches
    [(list 'regex (list 'regflag flag) ...)
     (build-regex-read-func 8096 flag)]
    [(list 'regex)
     (build-regex-read-func 8096 '())]
    [(list 'string16 "lestring16")
     (lambda ()
       (read-string16 (string-length (last compare-expr))))]
    [(list 'string16 "bestring16")
     (lambda ()
       (read-string16 (string-length (last compare-expr)) #t))]
    [(list 'numeric "byte")        (build-numeric-read-func signed-compare mask-expr read-byt)]
    [(list 'numeric "u" "byte")    (build-numeric-read-func #f mask-expr read-byt)]
    [(list 'numeric "short")       (build-numeric-read-func signed-compare mask-expr read-short)]
    [(list 'numeric "u" "short")   (build-numeric-read-func #f mask-expr read-short)]
    [(list 'numeric "long")        (build-numeric-read-func signed-compare mask-expr read-long)]
    [(list 'numeric "u" "long")    (build-numeric-read-func #f mask-expr read-long)]
    [(list 'numeric "quad")        (build-numeric-read-func signed-compare mask-expr read-quad)]
    [(list 'numeric "u" "quad")    (build-numeric-read-func #f mask-expr read-quad)]
    [(list 'numeric "float")       read-float]
    [(list 'numeric "double")      read-double]
    [(list 'numeric "leshort")     (build-numeric-read-func signed-compare mask-expr read-leshort)]
    [(list 'numeric "u" "leshort") (build-numeric-read-func #f mask-expr read-leshort)]
    [(list 'numeric "lelong")      (build-numeric-read-func signed-compare mask-expr read-lelong)]
    [(list 'numeric "u" "lelong")  (build-numeric-read-func #f mask-expr read-lelong)]
    [(list 'numeric "lequad")      (build-numeric-read-func signed-compare mask-expr read-lequad)]
    [(list 'numeric "u" "lequad")  (build-numeric-read-func #f mask-expr read-lequad)]
    [(list 'numeric "lefloat")     read-lefloat]
    [(list 'numeric "ledouble")    read-ledouble]
    [(list 'numeric "beshort")     (build-numeric-read-func signed-compare mask-expr read-beshort)]
    [(list 'numeric "u" "beshort") (build-numeric-read-func #f mask-expr read-beshort)]
    [(list 'numeric "belong")      (build-numeric-read-func signed-compare mask-expr read-belong)]
    [(list 'numeric "u" "belong")  (build-numeric-read-func #f mask-expr read-belong)]
    [(list 'numeric "bequad")      (build-numeric-read-func signed-compare mask-expr read-bequad)]
    [(list 'numeric "u" "bequad")  (build-numeric-read-func #f mask-expr read-bequad)]
    [(list 'numeric "befloat")     read-befloat]
    [(list 'numeric "bedouble")    read-bedouble]
    [(list 'numeric "date")        (build-numeric-read-func #t mask-expr read-long)]
    [(list 'numeric "ledate")      (build-numeric-read-func #t mask-expr read-lelong)]
    [(list 'numeric "bedate")      (build-numeric-read-func #t mask-expr read-belong)]
    [(list 'numeric "ldate")       (build-numeric-read-func #t mask-expr read-long)]
    [(list 'numeric "leldate")     (build-numeric-read-func #t mask-expr read-lelong)]
    [(list 'numeric "beldate")     (build-numeric-read-func #t mask-expr read-belong)]
    [(list 'numeric "qdate")       (build-numeric-read-func #t mask-expr read-quad)]
    [(list 'numeric "leqdate")     (build-numeric-read-func #t mask-expr read-lequad)]
    [(list 'numeric "beqdate")     (build-numeric-read-func #t mask-expr read-bequad)]
    [(list 'numeric "qldate")      (build-numeric-read-func #t mask-expr read-quad)]
    [(list 'numeric "leqldate")    (build-numeric-read-func #t mask-expr read-lequad)]
    [(list 'numeric "beqldate")    (build-numeric-read-func #t mask-expr read-bequad)]    
    [(list 'numeric "qwdate")      (build-numeric-read-func #t mask-expr read-quad)]
    [(list 'numeric "leqwdate")    (build-numeric-read-func #t mask-expr read-lequad)]
    [(list 'numeric "beqwdate")    (build-numeric-read-func #t mask-expr read-bequad)]
    [(list 'default "default")     (lambda () (not (test-passed-at-level)))]
    [_ (error (string-append "type expression doesn't match: " (~a type-expr)))]
    ))

(define (date-type-expr? expr)
  (if (and (list? expr)
           (>= (length expr) 2))
      (let ([type (second expr)])
        (and (string? type) (string-contains? type "date")))
      #f))

(define (char-case=? c match-char lci? uci?)
  (cond 
    [(and lci? (char-lower-case? match-char)) (char-ci=? c match-char)]
    [(and uci? (char-upper-case? match-char)) (char-ci=? c match-char)]
    [else (char=? c match-char)]))

; s and match-str must be the same length, but search-case-contains? ensures this
(define (search-case=? s match-str lci? uci?)
  (for/and ([c (in-string s)]
            [match-char (in-string match-str)])
    (char-case=? c match-char lci? uci?)))

(define (search-case-contains? s contained lc-insensitive? uc-insensitive? start-flag?)
  (define len (string-length contained))
  
  (for/first ([c (in-string s)]
              [i (in-range 0 (- (string-length s) (- len 1)))]
              #:when (and (char-case=? c (string-ref contained 0) lc-insensitive? uc-insensitive?)
                          (search-case=? (substring s i (+ i len)) contained lc-insensitive? uc-insensitive?)))
    ; if start-flag? is set, set the file offset to the start of the match,
    ; otherwise set the file offset to the byte after the match
    (if start-flag?
        (increment-file-position! i)
        (increment-file-position! (+ i len)))
    (substring s i (+ i len))))

(define (build-compare-func f)
  (lambda (s)
    (define result (f s))
    ;; test-passed-at-level is a parameter indicating if any test has passed at the
    ;; current level so far.
    (when result (test-passed-at-level #t))
    result))

(define (build-string-compare-func compare-str op lci-flag? uci-flag?)
  ;; creates a new, truncated string from s, or returns s if it is shorter than len
  (define (string-truncate s len)
    (define slen (string-length s))
    (if (< slen len)
        s
        (substring s 0 len)))
  
  (define len (string-length compare-str))
  
  (cond 
    [(string=? op "=")
     (cond 
       [(and lci-flag? uci-flag?)
        (build-compare-func
         (lambda (s) 
           (define truncated-str (string-truncate s len))
           (if (string-ci=? truncated-str compare-str)
               truncated-str
               #f)))]
       [else
        (build-compare-func
         (lambda (s) 
           (define truncated-str (string-truncate s len))
           (if (string=? truncated-str compare-str)
               truncated-str
               #f)))])]
    [(string=? op "!")
     (cond 
       [(and lci-flag? uci-flag?)
        (build-compare-func
         (lambda (s) 
           (define truncated-str (string-truncate s len))
           (if (not (string-ci=? truncated-str compare-str))
               truncated-str
               #f)))]
       [else
        (build-compare-func
         (lambda (s) 
           (define truncated-str (string-truncate s len))
           (if (not (string=? truncated-str compare-str))
               truncated-str
               #f)))])]
    [(string=? op "<")
     (cond 
       [(and lci-flag? uci-flag?)
        (build-compare-func
         (lambda (s) 
           (if (string-ci<? s compare-str) s #f)))]
       [else
        (build-compare-func
         (lambda (s) 
           (if (string<? s compare-str) s #f)))])]
    [(string=? op ">")
     (cond 
       [(and lci-flag? uci-flag?)
        (build-compare-func
         (lambda (s) 
           (if (string-ci>? s compare-str) s #f)))]
       [else
        (build-compare-func
         (lambda (s) 
           (if (string>? s compare-str) s #f)))])]
    [else (error (string-append "invalid string compare operator: " op))]))

(define (build-regex-compare-func compare-regex ci-flag? start-flag? line-flag?)
  (define re-str
    ;; wrap mode setting(s) around regex
    ;; all regex's will be run in multi-line mode
    (cond
      [ci-flag?
        (string-append "(?m:" "(?i:" compare-regex ")" ")")]
      [else
       (string-append "(?m:" compare-regex ")")]))
  
  (define re (pregexp re-str (lambda (emsg) #f)))

  ;; fallback to unix regular expression syntax
  (when (not re)
    (set! re (regexp re-str)))

  (build-compare-func
   (lambda (s)
     ;(eprintf "matching regex ~a~n" re-str)
     (define re-match-pos (regexp-match-positions re s))
     (if re-match-pos
         (let ([start-pos (caar re-match-pos)]
               [end-pos (cdar re-match-pos)])
           (if start-flag?
               (increment-file-position! start-pos)
               (increment-file-position! end-pos))
           (substring s start-pos end-pos))
         #f))))

(define (build-date-compare-func type-expr numeric-compare-func)
  (match type-expr
    [(or (list 'numeric "date")
         (list 'numeric "ledate")
         (list 'numeric "bedate")
         (list 'numeric "qdate")
         (list 'numeric "leqdate")
         (list 'numeric "beqdate"))
     (lambda (n)
       (seconds->date
        (numeric-compare-func n) #f))]
    [(or (list 'numeric "ldate")
         (list 'numeric "leldate")
         (list 'numeric "beldate")
         (list 'numeric "qldate")
         (list 'numeric "leqldate")
         (list 'numeric "beqldate"))
     (lambda (n)
       (seconds->date
        (numeric-compare-func n) #t))]
    [(or (list 'numeric "qwdate")
         (list 'numeric "leqwdate")
         (list 'numeric "beqwdate"))
     (lambda (n)
       (seconds->date
        ;; windows datetime unit is 100s of nanoseconds
        (- (/ (numeric-compare-func n) 10000000)
           windows-secs-to-epoch)
        #f))]))

;; returns a function to check the value read from the file
(define (compare compare-expr type-expr)
  (define strflags 
    (match type-expr
      [(list 'string8 "string" (list 'strflag flag) ...)
       flag]
      [(list 'search (list 'strflag flag) ...)
       (cons 'search flag)]
      [(list 'search (list 'srchcnt cnt) (list 'strflag flag) ...)
       (cons 'search flag)]
      [(list 'search (list 'strflag flag) ... (list 'srchcnt cnt))
       (cons 'search flag)]
      [(list 'regex (list 'regcnt cnt) (list 'regflag flag) ...)
       (cons 'regex flag)]
      [(list 'regex (list 'regflag flag) ... (list 'regcnt cnt))
       (cons 'regex flag)]
      [(list 'regex (list 'regflag flag) ...)
       (cons 'regex flag)]
      [(list 'regex (list 'regcnt cnt))
       (cons 'regex empty)]
      [(list 'regex)
       (cons 'regex empty)]
      [_ '()]))
  (define lci-flag? (member "c" strflags))
  (define uci-flag? (member "C" strflags))
  (define start-flag? (member "s" strflags))
  (define line-flag? (member "l" strflags))
  (define search? (member 'search strflags))
  (define regex? (member 'regex strflags))

  (define compare-func
    (match compare-expr
      [(list 'strtest x)
       (cond
         [search?
          (build-compare-func
           (lambda (s) 
             (search-case-contains? s x lci-flag? uci-flag? start-flag?)))]
         [regex?
          (build-regex-compare-func x lci-flag? start-flag? line-flag?)]
         [else
          (build-string-compare-func x "=" lci-flag? uci-flag?)])]
      [(list 'strtest op x) 
       (build-string-compare-func x op lci-flag? uci-flag?)]
      [(or (list 'numtest x)
           (list 'numtest "=" x))         
       (build-compare-func
        (lambda (n) 
          (if (= n x)
              n
              #f)))]
      [(list 'numtest "<" x)
       (build-compare-func
        (lambda (n) 
          (if (< n x)
              n
              #f)))]
      [(list 'numtest ">" x)
       (build-compare-func
        (lambda (n) 
          (if (> n x)
              n
              #f)))]
      [(list 'numtest "!" x) 
       (build-compare-func
        (lambda (n) 
          (if (not (= n x))
              n
              #f)))]
      ; the next three haven't been fully tested
      [(list 'numtest "&" x) 
       (build-compare-func
        (lambda (n) 
          (if (not (= (bitwise-and n x) 0))
              n
              #f)))]
      [(list 'numtest "^" x) 
       (build-compare-func
        (lambda (n)
          (if (not (bitwise-ior 
                    n 
                    (bitwise-and n x)))
              n
              #f)))]
      [(list 'numtest "~" x) 
       (build-compare-func
        (lambda (n) 
          (if (= n (bitwise-not x))
              n
              #f)))]
      [(list 'truetest "x") 
       ;; this test is designed to always return true for everything but the default type
       ;; the default type will read false if a test has already passed at the current level and true otherwise
       ;; truetest will pass a false value along, allowing the default test to fail
       (build-compare-func (lambda (n) n))]
      [_ (error (string-append "test expression doesn't match: " (~a compare-expr)))]))

  ;; date types use the normal numeric compare functions but will return a date
  (if (date-type-expr? type-expr)
      (build-date-compare-func type-expr compare-func)
      compare-func))

(define (single-cprintf-sub str val)
  ;; returns three values: the matched format specifier string, width, and precision
  ;; width and precision will be integers or false
  ;; a negative width indicates left justification. positive is right justification.
  ;; precision must be positive
  (define (get-format-modifiers s)
    (define format-mod-list (regexp-match #px"%(-?\\d*)\\.?(\\d*)[dxs]" s))
    (if format-mod-list
        (values (first format-mod-list)
                ;; if the width specifier is just '-' then return negative precision as the width
                ;; removed this for now since file doesn't seem to do this and not sure of C behaivor
                ;; when no width is specified
                #;(let ([second-match (second format-mod-list)]
                      [third-match (third format-mod-list)])
                  (if (and (string=? second-match "-") (string->number third-match))
                      (- (string->number third-match))
                      (string->number second-match)))
                (string->number (second format-mod-list))
                (string->number (third format-mod-list)))
        (values #f #f #f)))
  
  (define (format-mod str substitution specifier width precision)
    ;; return justified/padded string (need to check for width of #f)
    (define (justify str width pad-char)
      (define str-len (string-length str))
      (cond
        [(not (integer? width)) str]
        [(>= str-len (abs width)) str]
        [(> width 0)
         ; right justify
         (string-append (make-string (- width str-len) pad-char) str)]
        [else 
         ; left justify
         (string-append str (make-string (- (abs width) str-len) pad-char))]))

    ;(eprintf "specifier=~a substitution=~a width=~a, precision=~a~n" specifier substitution width precision) 
    ;; get the last character of the format specifier string 
    (define specifier-type
      (string-ref specifier (sub1 (string-length specifier))))
    (define pad-char
      (if (and (char=? specifier-type #\d) (string-contains? specifier "0"))
          #\0
          #\space))
    (cond 
      [(char=? specifier-type #\d)
       (define substitution-str (if (number? substitution)
                                    (number->string substitution)
                                    substitution))
       (define sub-length (string-length substitution-str))
       (define sub-string
         ;; the precision calcs for sub-string are wrong
         (if (or (not precision)
                 (<= precision sub-length))
             substitution-str
             (string-append (make-string (- precision sub-length) #\0) substitution-str))) 
       (format (string-replace str specifier "~a" #:all? #f)
               (justify sub-string width pad-char))]
      [(char=? specifier-type #\s)
       (define sub-length (string-length substitution))
       (define sub-string 
         (if (and (integer? precision)
                  (<= precision sub-length)) 
             (substring substitution 0 precision)
             substitution))
       (format (string-replace str specifier "~a" #:all? #f) (justify sub-string width pad-char))]
      [else  ;; fallback case
       (format (string-replace str specifier "~a" #:all? #f) substitution)]))

  (cond 
    [(string-contains? str "%d")
     (if (date*? val)
         (format (string-replace str "%d" "~a" #:all? #f)
                 (date->seconds val #t))
         (format (string-replace str "%d" "~a" #:all? #f) val))]
    [(string-contains? str "%u")
     (format (string-replace str "%u" "~a" #:all? #f) val)]    
    [(string-contains? str "%s")
     (if (date*? val)
         (format (string-replace str "%s" "~a" #:all? #f)
                 (date->string val #t))
         (format (string-replace str "%s" "~a" #:all? #f) val))]
    [(string-contains? str "%x") 
     (format (string-replace str "%x" "~x" #:all? #f) val)]
    [else
     (define-values (format-specifier width precision) (get-format-modifiers str))
     (if format-specifier
         (format-mod str val format-specifier width precision)
         str)]))

;; ex: (with-input-from-file "adventure.rkt" (lambda () (magic-test 0 (type '(string8 "string") '(strtest "MZ")) (compare '(strtest "MZ")) "dos executable")))
;; ex: (with-input-from-file "/tmp/iexplore.exe" (lambda () (magic-test (indoff 60 (size '(lelong ".l"))) (type '(string8 "string") '(strtest "PE\u0000\u0000")) (compare '(strtest "PE\u0000\u0000")) "PE executable (MS-Windows)")))
(define (magic-test off read-func compare-func [message ""])
  (with-handlers ([exn:fail? (lambda (exn) 
                               (print-warning "magic error: ~a~n" (exn-message exn))
                               #f)])
    (print-debug "running magic-test: ~a,~a,~a~n" off read-func compare-func)
    (file-position (current-input-port) off)
    (let* ([data (read-func)]
           [result (compare-func data)])
      ;; if the test is successful, print the message with any printf substitutions
      (when (and result (non-empty-string? message))
        (printf "~a " (single-cprintf-sub message result)))
      (when result
        (print-info "passed: ~a,~a,~a~n" off read-func compare-func))
      ;; return true or false to indicate the status of the test
      (if result
          #t
          #f))))
