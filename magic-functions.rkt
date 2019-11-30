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
;(provide offset)
(provide type)
(provide compare)
(provide indoff)
(provide read-byt read-short read-long read-quad read-float read-double)
(provide read-leshort read-lelong read-lequad read-lefloat read-ledouble)
(provide read-beshort read-belong read-bequad read-befloat read-bedouble)

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

;; reads until a null byte is found
;; returns a string in latin-1 encoding
(define (read-cstring8)
  (let loop ((result #""))
    (let ((next-char (read-byte)))
      (if (or (eof-object? next-char) (= next-char 0))
          (bytes->string/latin-1 result)
          (loop (bytes-append result (bytes next-char)))))))

;; I don't think the null terminated read works with magic. The null bytes must be made explicit in the comparison.
(define (read-string8 len)
  (let ([data (read-bytes len)])
    (if (eof-object? data)
        (error "eof")
        (bytes->string/latin-1 data))))

;; read a minimum of len bytes/characters from the current input port
;; if bytestring read doesn't end in a null byte, read until one is reached (read-cstring8 does this)
(define (read-string8-till-null len)
  (define data (read-bytes len))
  (when (eof-object? data) (error "eof"))
  
  (define datalen (bytes-length data))
  (cond 
    [(< datalen len)
     (string-append (bytes->string/latin-1 data) (read-cstring8))]
    [(= (bytes-ref data (sub1 datalen)) 0)
     (bytes->string/latin-1 data)]
    [else
     (string-append (bytes->string/latin-1 data) (read-cstring8))]))

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

;; the search function should leave the file position set to its original value,
;; i.e. where the search began. then the compare function will update the 
;; file offset to either the beginning or end of the match, depending on the
;; flags.
(define (build-search-read-func cnt flags needle)
  (let ([len (+ cnt (string-length needle))]
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
      [(list 'numeric _ _ (list 'nummask (list 'op op) x)) (list op x)]
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
    [(list 'search (list 'srchcnt cnt) (list 'strflag flag) ...)
     (build-search-read-func cnt flag (last compare-expr))]
    [(list 'search (list 'strflag flag) ... (list 'srchcnt cnt))
     (build-search-read-func cnt flag (last compare-expr))]
    [(list 'search (list 'strflag flag) ...)
     ; default count to 4096 for now, but need to read the entire file if necessary
     ; after the match is found, should the file offset be set to the end of the match?
     (build-search-read-func 4096 flag (last compare-expr))]
    [(list 'search) (build-search-read-func 4096 '() (last compare-expr))]
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
    [(list 'default "default")     (lambda () 42)] ; could return anything
    [_ (error (string-append "type expression doesn't match: " (~a type-expr)))]
    ))

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
    #t))
    ;(list i c)))

(define (build-string-compare-func compare-str op ci-flag? lci-flag? uci-flag?)
  ;; creates a new, truncated string from s, or returns s if it is shorter than len
  (define (string-truncate s len)
    (define slen (string-length s))
    (if (< slen len)
        s
        (substring s 0 len)))
  
  (define len (string-length compare-str))
  
  (cond 
    [(string=? op "=")
     (if ci-flag? 
         (lambda (s) 
           (string-ci=? (string-truncate s len) compare-str))
         (lambda (s) 
           (string=? (string-truncate s len) compare-str)))]
    [(string=? op "<")
     (if ci-flag? 
         (lambda (s) 
           (string-ci<? (string-truncate s len) compare-str))
         (lambda (s) 
           (string<? (string-truncate s len) compare-str)))]
    [(string=? op ">")
     (if ci-flag? 
         (lambda (s) 
           (string-ci>? (string-truncate s len) compare-str))
         (lambda (s) 
           (string>? (string-truncate s len) compare-str)))]))

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
      [_ '()]))
  (define ci-flag? (and (member "c" strflags) (member "C" strflags)))
  (define lci-flag? (member "c" strflags))
  (define uci-flag? (member "C" strflags))
  (define start-flag? (member "s" strflags))
  (define search? (member 'search strflags))

  (match compare-expr
    [(list 'strtest x)
     (if search?
         (lambda (s) (search-case-contains? s x lci-flag? uci-flag? start-flag?))
         (build-string-compare-func x "=" ci-flag? lci-flag? uci-flag?))]
    [(list 'strtest "<" x) 
     (build-string-compare-func x "<" ci-flag? lci-flag? uci-flag?)]
    [(list 'strtest ">" x) 
     (build-string-compare-func x ">" ci-flag? lci-flag? uci-flag?)]
    [(list 'strtest "=" x) 
     (build-string-compare-func x "=" ci-flag? lci-flag? uci-flag?)]
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
  ;; returns three values: the matched format specifier string, width, and precision
  ;; width and precision will be integers or false
  ;; a negative width indicates left justification. positive is right justification.
  ;; precision must be positive
  (define (get-format-modifiers s)
    (define format-mod-list (regexp-match #px"%(-?\\d*)\\.(\\d*)[dxs]" s))
    (if format-mod-list
        (values (first format-mod-list)
                (string->number (second format-mod-list))
                (string->number (third format-mod-list)))
        (values #f #f #f)))
  
  (define (format-mod str substr specifier width precision)
    ;(eprintf "substr=~a width=~a, precision=~a~n" substr width precision) 
    (format (string-replace str specifier "~a" #:all? #f) substr))

  (cond 
    [(string-contains? str "%d") 
     (format (string-replace str "%d" "~a" #:all? #f) val)]
    [(string-contains? str "%s") 
     (format (string-replace str "%s" "~a" #:all? #f) val)]
    [(string-contains? str "%x") 
     (format (string-replace str "%x" "~x" #:all? #f) val)]
    [else
     (define-values (format-specifier width precision) (get-format-modifiers str))
     (if (or width precision)
         (format-mod str val format-specifier width precision)
         str)]))

;; ex: (with-input-from-file "adventure.rkt" (lambda () (magic-test 0 (type '(string8 "string") '(strtest "MZ")) (compare '(strtest "MZ")) "dos executable")))
;; ex: (with-input-from-file "/tmp/iexplore.exe" (lambda () (magic-test (indoff 60 (size '(lelong ".l"))) (type '(string8 "string") '(strtest "PE\u0000\u0000")) (compare '(strtest "PE\u0000\u0000")) "PE executable (MS-Windows)")))
(define (magic-test off read-func compare-func [message ""])
  (with-handlers ([exn:fail? (lambda (exn) 
                               (eprintf "magic error: ~a~n" (exn-message exn))
                               #f)])
    ;(printf "running magic-test: ~a,~a,~a~n" off read-func compare-func)
    ;(display off)
    ;(display read-func)
    ;(display compare-func)
    (file-position (current-input-port) off)
    (let* ([data (read-func)]
           [result (compare-func data)])
      ;(eprintf "data=~a, result=~a~n" data result)
      (when (and result (non-empty-string? message))
        (printf "~a " (single-cprintf-sub message data)))
      result)))
