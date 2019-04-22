#lang racket/base

(require brag/support)
(require syntax/strip-context)
(require racket/string)
(require "parser.rkt")
;(require "magic-functions.rkt")

(provide read-syntax)
(provide make-tokenizer)

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port)))
  (strip-context #`(module magic-mod "expander.rkt"
                          #,parse-tree)))

(define-lex-abbrev hws (:+ " " "\t"))
(define-lex-abbrev digits (:+ (char-set "0123456789")))
(define-lex-abbrev hex-digits (:+ (char-set "0123456789abcdefABCDEF")))
(define-lex-abbrev op (:= 1 (char-set "<>=&^!+-*/%|")))
(define-lex-abbrev paren (:= 1 (char-set "()")))
;(define-lex-abbrev string-chars (complement (:+ " " "\t" "\n")))
(define-lex-abbrev string-chars (:+ (char-set "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-.") "\\"))
(define-lex-abbrev key-word (:or "byte" "short" "beshort" "leshort" "long" "belong" "lelong" "quad" "bequad" "lequad" "string" "search" "regex"))
(define-lex-abbrev integer-type (:or "byte" "short" "beshort" "leshort" "long" "belong" "lelong" "quad" "bequad" "lequad"))
(define-lex-abbrev string-type (:or "string" "search" "regex"))
(define-lex-abbrev size-specifier (:= 1 (char-set "bBcCshSHlLm")))

(define (raw-string-to-racket s)
  ;(string-replace s "\\0" "\u0000"))
  ; convert raw string bytes to racket unicode representation
  ;(regexp-replace* #px"\\\\(\\d{1,2})" s "\u00\\2"))
  (define (escapable-char? c)
    (and (not (eof-object? c))
         (or (char=? c #\a) (char=? c #\b) (char=? c #\f) (char=? c #\n) (char=? c #\r) (char=? c #\t) (char=? c #\v) 
             (char=? c #\\) (char=? c #\0) (char=? c #\space)
             )))
  (define (escape c)
    (cond [(char=? c #\n) "\n"]
          [(char=? c #\0) "\0"]
          [(char=? c #\space) " "]
          [(char=? c #\t) "\t"]
          [(char=? c #\\) "\\"]))

  (define sp (open-input-string s))
  (let loop ([new-string ""]
             [c (read-char sp)])
    (cond 
      [(eof-object? c) new-string]
      [(and (char=? c #\\) (escapable-char? (peek-char sp)))
       (loop (string-append new-string (escape (read-char sp)))
             (read-char sp))]
      [else (loop (string-append new-string (string c))
                  (read-char sp))])))
    
(define magic-lexer
  (lexer-srcloc
   ;[(char-set "><-.,+[]") lexeme]
   [(from/to "#" "\n")
    (begin
      (set! hws-count 0)
      (token 'COMMENT #:skip? #t))]
   [(from/to "!:" "\n")
    (begin
      (set! hws-count 0)
      (token 'MIME #:skip? #t))]
   ["\n" 
    (begin
      (displayln "newline found1") 
      (set! hws-count 0)
      (token 'EOL))]
   [hws 
    (begin 
      (set! hws-count (add1 hws-count))
      (token 'HWS #:skip? #f))]
   ;[">" (token 'LEVEL)]
   [op (token lexeme lexeme)]
   [paren (token lexeme lexeme)]
   [(:seq "." size-specifier) (token lexeme lexeme)]
   [string-type 
    (begin
      (set! current-lexer magic-lexer-string-flags)
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
   ;[string-chars (token 'STRING (raw-string-to-racket lexeme))]
   ;[any-char (token 'CHAR lexeme)]))
))

(define magic-lexer-message
  (lexer-srcloc
   ["\n" 
    (begin
      (displayln "newline found2") 
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
   [digits (token 'INTEGER (string->number lexeme))]
   [hws
    (begin 
      (set! hws-count (add1 hws-count))
      (set! current-lexer magic-lexer-string)
      (token 'HWS #:skip? #f))]))

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
      (printf "running string helper, lexeme = ~a~n" lexeme)
      (token 'STRING (raw-string-to-racket (string-append lexeme (magic-lexer-string-helper input-port)))))]
   ;; the simple case with no escaped space characters
   [(from/stop-before (:~ " " "\t")  (:or " " "\t" "\n"))
    (begin
      (set! current-lexer magic-lexer)
      (printf "lexeme read as: ~a, converted to: ~a~n" lexeme (raw-string-to-racket lexeme))
      (token 'STRING (raw-string-to-racket lexeme)))]))


(define hws-count 0)
(define current-lexer magic-lexer)

(define (make-tokenizer port [path #f])
  (port-count-lines! port)
  (lexer-file-path path)
  (define (next-token)
    (cond [(= hws-count 3)
           (begin
             (displayln "calling lexer2")
             (magic-lexer-message port))]
          [else 
           (current-lexer port)]))
  next-token)

;; test function
(define (lex str)
  (apply-lexer magic-lexer str))
  ;(magic-lexer (open-input-string str)))
