#lang racket

(require brag/support)
(require "parser.rkt")

(provide read-syntax)

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port)))
  (define module-datum `(module magic-mod "expander.rkt"
                          ,parse-tree))
  (datum->syntax #f module-datum))

(define-lex-abbrev digits (:+ (char-set "0123456789")))
(define-lex-abbrev hex-digits (:+ (char-set "0123456789abcedfABCDEF")))

(define magic-lexer
  (lexer-srcloc
   ;[(char-set "><-.,+[]") lexeme]
   [(from/to "#" "\n") (token 'COMMENT #:skip? #t)]
   ["\n" (token 'EOL)]
   [whitespace (token 'HWS #:skip? #f)]
   [">" (token 'LEVEL)]
   [(:or "beshort" "byte") (token lexeme lexeme)]
   [digits (token 'INTEGER (string->number lexeme))]
   [(:seq "0x" hex-digits) (token 'INTEGER (string->number (substring lexeme 2) 16))]
   ;[any-string (token 'STRING lexeme)]
   [any-char (token 'CHAR lexeme)]))

(define (make-tokenizer port [path #f])
  (port-count-lines! port)
  (lexer-file-path path)
  (define (next-token)
    (magic-lexer port))
  next-token)

;; test function
(define (lex str)
  (apply-lexer magic-lexer str))
  ;(magic-lexer (open-input-string str)))
