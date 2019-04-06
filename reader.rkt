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

(define-lex-abbrev digits (:+ (char-set "0123456789")))
(define-lex-abbrev hex-digits (:+ (char-set "0123456789abcdefABCDEF")))
(define-lex-abbrev op (:= 1 (char-set "<>=&^!+-*/%|")))
(define-lex-abbrev paren (:= 1 (char-set "()")))
(define-lex-abbrev string-chars (:+ (char-set "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789") "\\"))
(define-lex-abbrev key-word (:or "byte" "short" "beshort" "leshort" "long" "belong" "lelong" "quad" "bequad" "lequad" "string" "search" "regex"))
(define-lex-abbrev integer-type (:or "byte" "short" "beshort" "leshort" "long" "belong" "lelong" "quad" "bequad" "lequad"))
(define-lex-abbrev size-specifier (:= 1 (char-set "bBcCshSHlLm")))

(define (magic-string-to-racket s)
  ;(string-replace s "\\0" "\u0000"))
  ; convert raw string bytes to racket unicode representation
  (regexp-replace* #px"\\\\(\\d{1,2})" s "\u00\\2"))

(define hws-count 0)

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
   [(:+ " " "\t") 
    (begin 
      (set! hws-count (add1 hws-count))
      (token 'HWS #:skip? #f))]
   ;[">" (token 'LEVEL)]
   [op (token lexeme lexeme)]
   [paren (token lexeme lexeme)]
   [(:seq "." size-specifier) (token lexeme lexeme)]
   [key-word (token lexeme lexeme)]
   [(:seq "u" integer-type) (let ([pos (file-position input-port)])
                              (file-position input-port (- pos 
                                                           (- (string-length lexeme) 
                                                              1)))
                              (token "u" "u"))]
   [digits (token 'INTEGER (string->number lexeme))]
   [(:seq "0x" hex-digits) (token 'INTEGER (string->number (substring lexeme 2) 16))]
   [string-chars (token 'STRING (magic-string-to-racket lexeme))]
   ;[any-char (token 'CHAR lexeme)]))
))

(define magic-lexer2
  (lexer-srcloc
   ["\n" 
    (begin
      (displayln "newline found2") 
      (set! hws-count 0)
      (token 'EOL))]
   [(from/stop-before any-char "\n") 
    (begin
      (set! hws-count 0)
      (token 'STRING lexeme))]))
   ;[any-string (token 'STRING lexeme)]))

(define magic-lexer-string
  (lexer-srcloc
   [string-chars (token 'STRING lexeme)]))

(define (make-tokenizer port [path #f])
  (port-count-lines! port)
  (lexer-file-path path)
  (define (next-token)
    (cond [(= hws-count 3)
           (begin
             (displayln "calling lexer2")
             (magic-lexer2 port))]
          [else 
           (magic-lexer port)]))
  next-token)

;; test function
(define (lex str)
  (apply-lexer magic-lexer str))
  ;(magic-lexer (open-input-string str)))