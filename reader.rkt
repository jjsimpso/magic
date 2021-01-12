#lang racket/base

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

(require brag/support)
(require syntax/strip-context)
(require syntax/stx)
(require racket/string)

(require "parser.rkt")
(require "output.rkt")

(provide magic-read-syntax)
(provide magic-read-syntax-raw)
(provide make-tokenizer)

;(set-magic-verbosity! 'info)

(define (magic-read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port)))
  (strip-context #`(module magic-mod magic/expander
                          #,parse-tree)))

;; experimental function to be called when using include/reader
;; never got this to work
(define (magic-read-syntax-raw path port)
  (define parse-tree (parse path (make-tokenizer port)))
  (stx-car (strip-context #`(#,parse-tree))))

;; old version that is probably not what we want
#;(define (magic-read-syntax-raw path port)
  (define parse-tree
    (stx-cdr
     (stx-cdr
      (parse path (make-tokenizer port)))))
  (eprintf "calling magic-read-syntax-raw~n")
  ;(eprintf "parse tree = ~a~n" parse-tree)
  (datum->syntax #'here (strip-context parse-tree)))

(define-lex-abbrev hws (:+ " " "\t"))
(define-lex-abbrev eol (:+ "\n"))
(define-lex-abbrev digits (:+ (char-set "0123456789")))
(define-lex-abbrev hex-digits (:+ (char-set "0123456789abcdefABCDEF")))
(define-lex-abbrev op (:= 1 (char-set "<>=&^!+-*/%|")))
(define-lex-abbrev paren (:= 1 (char-set "()")))
;(define-lex-abbrev string-chars (complement (:+ " " "\t" "\n")))
(define-lex-abbrev string-chars (:+ (char-set "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-.") "\\"))
(define-lex-abbrev string-compare (:= 1 (char-set "<>=!")))
(define-lex-abbrev string-flag (:= 1 (char-set "WwcCtbT")))
(define-lex-abbrev pstring-flag (:= 1 (char-set "BHhLlJ")))
(define-lex-abbrev search-flag (:= 1 (char-set "WwcCtbTsl")))
(define-lex-abbrev indirect-flag (:= 1 (char-set "r")))
(define-lex-abbrev key-word (:or "byte" "short" "beshort" "leshort" "long" "belong" "lelong" "quad" "bequad" "lequad" "date" "bedate" "ledate" "medate" "ldate" "beldate" "leldate" "meldate" "qdate" "beqdate" "leqdate" "qldate" "beqldate" "leqldate" "qwdate" "beqwdate" "leqwdate" "string" "lestring16" "bestring16" "pstring" "search" "regex" "default" "clear" "x" "indirect"))
(define-lex-abbrev integer-type (:or "byte" "short" "beshort" "leshort" "long" "belong" "lelong" "quad" "bequad" "lequad"))
(define-lex-abbrev string-type (:or "string" "lestring16" "bestring16"))
(define-lex-abbrev unsupported-type (:or "leid3" "beid3" "der"))
(define-lex-abbrev sign-specifier (:= 1 (char-set ".,")))
(define-lex-abbrev size-specifier (:= 1 (char-set "bBcCshSHlLm")))

(define (raw-string-to-racket s)
  ;(string-replace s "\\0" "\u0000"))
  ; convert raw string bytes to racket unicode representation
  ;(regexp-replace* #px"\\\\(\\d{1,2})" s "\u00\\2"))
  ;(eprintf "converting string ~a~n" s)
  
  (define (escapable-char? c)
    (and (not (eof-object? c))
         (or (char=? c #\a) (char=? c #\b) (char=? c #\f) (char=? c #\n) (char=? c #\r) (char=? c #\t) (char=? c #\v) 
             (char=? c #\\) (char=? c #\space)
             )))
  (define (escape c)
    (cond [(char=? c #\n) "\n"]
          [(char=? c #\a) "\a"]
          [(char=? c #\b) "\b"]
          [(char=? c #\f) "\f"]
          [(char=? c #\r) "\r"]
          [(char=? c #\t) "\t"]
          [(char=? c #\v) "\v"]
          [(char=? c #\space) " "]
          [(char=? c #\0) "\0"]
          [(char=? c #\\) "\\"]))

  (define (hex-start? c)
    (and (not (eof-object? c)) 
         (char=? c #\x)))
  (define (read-hex-number in)
    ;; read the two characters after '\x' in a string and convert to character of the form #\u00xx
    (integer->char (string->number (read-string 2 in) 16)))

  (define (octal? c)
    (and (not (eof-object? c)) 
         (char>=? c #\0) (char<=? c #\7)))
  (define (read-octal-number in)
    ;; octal numbers can be from 1 to 3 digits
    ;; peek ahead to find the length
    (define len
      (for/sum ([i (in-range 0 3)]
                #:break (not (octal? (peek-char in i))))
        1))
    (integer->char (string->number (read-string len in) 8)))

  (define sp (open-input-string s))
  (let loop ([new-string-rlist '()]
             [c (read-char sp)])
    (cond 
      [(eof-object? c) (string-append* (reverse new-string-rlist))]
      [(and (char=? c #\\) (escapable-char? (peek-char sp)))
       (loop (cons (escape (read-char sp)) new-string-rlist)
             (read-char sp))]
      [(and (char=? c #\\) (hex-start? (peek-char sp)))
       ; discard the 'x' character
       (read-char sp)
       (loop (cons (string (read-hex-number sp)) new-string-rlist)
             (read-char sp))]
      [(and (char=? c #\\) (octal? (peek-char sp)))
       (loop (cons (string (read-octal-number sp)) new-string-rlist)
             (read-char sp))]
      [(char=? c #\\)
       ; discard the '\' character
       (loop new-string-rlist (read-char sp))]
      [else (loop (cons (string c) new-string-rlist)
                  (read-char sp))])))

(define magic-lexer
  (lexer-srcloc
   ;[(char-set "><-.,+[]") lexeme]
   [(from/to "#" eol)
    (begin
      (set! hws-count 0)
      (token 'COMMENT #:skip? #t))]
   [(from/to "!:" eol)
    (begin
      (set! hws-count 0)
      (token 'MIME #:skip? #t))]
   [eol
    (begin
      (print-debug "newline found1~n") 
      (set! hws-count 0)
      (token 'EOL))]
   [hws 
    (begin 
      (set! hws-count (add1 hws-count))
      (token 'HWS #:skip? #f))]
   ;[">" (token 'LEVEL)]
   [op (token lexeme lexeme)]
   [paren
    (begin
      (set! current-lexer magic-lexer-indirect-offset)
      (token lexeme lexeme))]
   ;[(:seq "." size-specifier) (token lexeme lexeme)]
   [string-type
    (begin
      (set! current-lexer magic-lexer-string-flags)
      (token lexeme lexeme))]
   ["pstring"
    (begin
      (set! current-lexer magic-lexer-pstring-flags)
      (token lexeme lexeme))]
   ["search"
    (begin
      (set! current-lexer magic-lexer-search-flags)
      (token lexeme lexeme))]
   ["regex"
    (begin
      (set! current-lexer magic-lexer-search-flags)
      (token lexeme lexeme))]
   ["name"
    (begin
      (set! current-lexer magic-lexer-name)
      (token lexeme lexeme))]
   ["use"
    (begin
      (set! current-lexer magic-lexer-name)
      (token lexeme lexeme))]
   ["indirect"
    (begin
      (set! current-lexer magic-lexer-indirect-flags)
      (token lexeme lexeme))]
   [key-word (token lexeme lexeme)]
   [(:seq "u" integer-type) (let ([pos (file-position input-port)])
                              (file-position input-port (- pos 
                                                           (- (string-length lexeme) 
                                                              1)))
                              (token "u" "u"))]
   [digits (token 'INTEGER (string->number lexeme))]
   [(:seq "-" digits) (token 'INTEGER (string->number lexeme))]
   [(:seq "0x" hex-digits) (token 'INTEGER (string->number (substring lexeme 2) 16))]
   [unsupported-type
    (begin
      (error (format "Unsupported type \"~a\" on line ~a in ~a" lexeme (line lexeme-start) (lexer-file-path))))]

   ;[string-chars (token 'STRING (raw-string-to-racket lexeme))]
   ;[any-char (token 'CHAR lexeme)]))
))

(define magic-lexer-indirect-offset
  (lexer-srcloc
   [")"
    (begin
      (set! current-lexer magic-lexer)
      (token lexeme lexeme))]
   ["&" (token lexeme lexeme)]
   [(:seq sign-specifier size-specifier)
    (begin
      (set! current-lexer magic-lexer-indirect-offset-op)
      (token lexeme lexeme))]
   [digits (token 'INTEGER (string->number lexeme))]
   [(:seq "-" digits) (token 'INTEGER (string->number lexeme))]
   [(:seq "0x" hex-digits) (token 'INTEGER (string->number (substring lexeme 2) 16))]))

(define magic-lexer-indirect-offset-op
  (lexer-srcloc
   ["("
    (begin
      (set! current-lexer magic-lexer-nested-indirect-offset)
      (token lexeme lexeme))]
   [")"
    (begin
      (set! current-lexer magic-lexer)
      (token lexeme lexeme))]
   [op (token lexeme lexeme)]
   [digits (token 'INTEGER (string->number lexeme))]
   [(:seq "0x" hex-digits) (token 'INTEGER (string->number (substring lexeme 2) 16))]))

(define magic-lexer-nested-indirect-offset
  (lexer-srcloc
   [")"
    (begin
      (set! current-lexer magic-lexer-indirect-offset)
      (token lexeme lexeme))]
   [digits (token 'INTEGER (string->number lexeme))]
   [(:seq "-" digits) (token 'INTEGER (string->number lexeme))]
   [(:seq "0x" hex-digits) (token 'INTEGER (string->number (substring lexeme 2) 16))]))

(define magic-lexer-message
  (lexer-srcloc
   [eol 
    (begin
      (print-debug "newline found2~n") 
      (set! hws-count 0)
      (token 'EOL))]
   [(from/stop-before (:~ "\n") "\n") 
    (begin
      (set! hws-count 0)
      (token 'STRING (raw-string-to-racket lexeme)))]))
   ;[any-string (token 'STRING lexeme)]))

(define magic-lexer-string-flags
  (lexer-srcloc
   ["/" (token lexeme lexeme)]
   [string-flag (token lexeme lexeme)]
   [hws
    (let ([next-char (peek-char input-port)])
      (set! hws-count (add1 hws-count))
      ;; check for optional comparison operator
      (if (or (char=? next-char #\<) (char=? next-char #\>) (char=? next-char #\=) (char=? next-char #\!))
          (set! current-lexer magic-lexer-string-compare)
          (set! current-lexer magic-lexer-string))
      (token 'HWS #:skip? #f))]))

(define magic-lexer-pstring-flags
  (lexer-srcloc
   ["/" (token lexeme lexeme)]
   [pstring-flag (token lexeme lexeme)]
   [hws
    (let ([next-char (peek-char input-port)])
      (set! hws-count (add1 hws-count))
      ;; check for optional comparison operator
      (if (or (char=? next-char #\<) (char=? next-char #\>) (char=? next-char #\=) (char=? next-char #\!))
          (set! current-lexer magic-lexer-string-compare)
          (set! current-lexer magic-lexer-string))
      (token 'HWS #:skip? #f))]))

(define magic-lexer-search-flags
  (lexer-srcloc
   ["/" (token lexeme lexeme)]
   [digits (token 'INTEGER (string->number lexeme))]
   [(:seq "0x" hex-digits) (token 'INTEGER (string->number (substring lexeme 2) 16))]
   [search-flag (token lexeme lexeme)]
   [hws
    (let ([next-char (peek-char input-port)])
      (set! hws-count (add1 hws-count))
      ;; discard the initial '=' character in search or regex test values
      (when (char=? next-char #\=) (read-char input-port))
      (set! current-lexer magic-lexer-string)
      (token 'HWS #:skip? #f))]))

(define magic-lexer-indirect-flags
  (lexer-srcloc
   ["/" (token lexeme lexeme)]
   [indirect-flag (token lexeme lexeme)]
   [hws
    (begin
      (set! hws-count (add1 hws-count))
      (set! current-lexer magic-lexer)
      (token 'HWS #:skip? #f))]))

;; get comparison operator
(define magic-lexer-string-compare
  (lexer-srcloc
   [string-compare
    (begin
      (set! current-lexer magic-lexer-string)
      (token lexeme lexeme))]
   [any-char
    (begin 
      (set! current-lexer magic-lexer-string)
      (error "Not a string comparison operator"))]))

(define magic-lexer-string-helper
  (lexer
   [(:or " " "\t" "\n")
    (begin
      (file-position input-port (sub1 (file-position input-port)))
      "")]
   [(:seq (:* (:~ " " "\t" "\n")) (:seq "\\" any-char)) 
    (string-append lexeme (magic-lexer-string-helper input-port))]
   [(from/stop-before (:~ " " "\t" "\n" "\\") (:or " " "\t" "\n"))
    lexeme]))

(define magic-lexer-string
  (lexer-srcloc
   ;; include escaped space characters in the string
   ;; always longer than the case below because it includes the escaped space character in the match
   [(:seq (:* (:~ " " "\t" "\n")) (:seq "\\" " ")) 
    (begin
      (set! current-lexer magic-lexer)
      (print-debug "running string helper, lexeme = ~a~n" lexeme)
      (token 'STRING (raw-string-to-racket (string-append lexeme (magic-lexer-string-helper input-port)))))]
   ;; treat "x" as a truetest
   [(:seq "x" (:or " " "\t" "\n"))
    (begin
      ;; set position to before the HWS character
      (file-position input-port (sub1 (file-position input-port)))
      (set! current-lexer magic-lexer)
      (token "x" "x"))]
   ;; the simple case with no escaped space characters
   [(from/stop-before (:~ " " "\t")  (:or " " "\t" "\n"))
    (begin
      (set! current-lexer magic-lexer)
      (print-info "lexeme read as: ~a, converted to: ~a~n" lexeme (raw-string-to-racket lexeme))
      (token 'STRING (raw-string-to-racket lexeme)))]))

(define magic-lexer-name
  (lexer-srcloc
   [hws
    (begin
      (set! hws-count (add1 hws-count))
      (token 'HWS #:skip? #f))]
   [(from/stop-before (:~ " " "\t") (:or " " "\t" "\n")) 
    (begin
      (print-debug "running magic-lexer-name, lexeme = ~a~n" lexeme)
      (set! current-lexer magic-lexer)
      ;; use trim-ends to take the leading backslash off the lexeme if it exists.
      ;; some magic backslash escapes the leading '^' for reverse names. we don't require
      ;; this but trimming it here allows us to support it.
      (token 'MAGIC-NAME (string->symbol (trim-ends "\\" lexeme ""))))]))

(define hws-count 0)
(define current-lexer magic-lexer)

(define (make-tokenizer port [path #f])
  (port-count-lines! port)
  (lexer-file-path path)
  (define (next-token)
    (cond [(= hws-count 3)
           (begin
             (print-debug "calling message lexer~n")
             (magic-lexer-message port))]
          [else 
           (current-lexer port)]))  
  next-token)

;; test function
(define (lex str)
  (apply-lexer magic-lexer str))
