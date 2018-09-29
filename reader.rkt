#lang racket/base

(require brag/support)
(require syntax/strip-context)
(require "parser.rkt")

(provide read-syntax)
(provide make-tokenizer)

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port)))
  (strip-context #`(module magic-mod "expander.rkt"
                          #,parse-tree)))

(define-lex-abbrev digits (:+ (char-set "0123456789")))
(define-lex-abbrev hex-digits (:+ (char-set "0123456789abcdefABCDEF")))
(define-lex-abbrev op (:= 1 (char-set "<>=&^!")))
(define-lex-abbrev string-chars (:+ (char-set "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")))
(define-lex-abbrev key-word (:or "beshort" "leshort" "byte" "short" "string"))

(define hws-count 0)

(define magic-lexer
  (lexer-srcloc
   ;[(char-set "><-.,+[]") lexeme]
   [(from/to "#" "\n")
    (begin
      (set! hws-count 0)
      (token 'COMMENT #:skip? #t))]
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
   [key-word (token lexeme lexeme)]
   [(:seq "u" key-word) (let ([pos (file-position input-port)])
                          (file-position input-port (- pos 
                                                       (- (string-length lexeme) 
                                                          1)))
                          (token "u" "u"))]
   [digits (token 'INTEGER (string->number lexeme))]
   [(:seq "0x" hex-digits) (token 'INTEGER (string->number (substring lexeme 2) 16))]
   [string-chars (token 'STRING lexeme)]
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
