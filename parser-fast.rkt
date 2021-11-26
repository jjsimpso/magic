#lang racket/base

;;Copyright 2021 Jonathan Simpson
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
(require syntax/stx)

(require "reader.rkt")

(provide parse
         parse-to-datum)

;; Impelment a recursive descent parser

(define list-of-tokens '())


;; helper functions
;; ----------------

(define (peek-token)
  (and (not (null? list-of-tokens))
       (car list-of-tokens)))

(define (pop-token)
  (define tkn (peek-token))
  (when tkn
    (set! list-of-tokens (cdr list-of-tokens)))
  tkn)

(define (push-token tkn)
  (set! list-of-tokens (cons tkn list-of-tokens)))

(define (token-type tkn)
  (token-struct-type (srcloc-token-token tkn)))

(define (token-val tkn)
  (token-struct-val (srcloc-token-token tkn)))

(define (token-eq? tkn sym)
  (if tkn
      (eq? (token-type tkn) sym)
      #f))

(define (next-token-eq? sym)
  (and (peek-token)
       (eq? (token-type (peek-token)) sym)))

(define (size-token? tkn)
  (case (token-type tkn)
    [(.B .b .C .c .s .h .S .H .l .L .m) #t]
    [(|,B| |,b| |,C| |,c| |,s| |,h| |,S| |,H| |,l| |,L| |,m|) #t]
    [else #f]))

(define (size-op-token? tkn)
  (case (token-type tkn)
    [(~ + - * / % & || ^) #t]
    [else #f]))

(define (try-rule rule-func)
  (define saved-tokens-list list-of-tokens)
  (with-handlers ([exn:fail? (lambda (exn)
                               ;; on error, restore token list and return false
                               (set! list-of-tokens saved-tokens-list)
                               #f)])
    (rule-func)))

(define (parse-error str)
  ;(eprintf str)
  ;(eprintf "~n")
  (error str))

;; read all the tokens from the function token-source and return a list of tokens
(define (get-all-tokens token-source next-token tokens)
  (if (eof-object? next-token)
      (reverse tokens)
      (get-all-tokens token-source (token-source) (cons next-token tokens))))


;; Interface functions
;; -------------------

;; token-source is a function that returns the next token or a list of tokens
(define (parse path token-source)
  (if (procedure? token-source)
      (set! list-of-tokens (get-all-tokens token-source (token-source) '()))
      (set! list-of-tokens token-source))
  (with-handlers ([exn:fail? (lambda (exn) 
                               (eprintf "magic parse error: ~a~n" (exn-message exn))
                               #f)])
    (magic)))

(define (parse-to-datum path token-source)
  (syntax->datum (parse path token-source)))


;; Grammar Rules
;; -------------

;; originating rule
(define (magic)
  (let loop ([tkn (pop-token)]
             [magic-stx #'()])
    (cond
      [(not tkn) magic-stx]
      [(eq? (token-type tkn) 'EOL)
       (loop (pop-token) magic-stx)]
      [else
       (define q (or (try-rule query) (try-rule named-query)))
       (if (syntax? q)
           (loop (pop-token) #`(#,magic-stx #,q))
           (parse-error "magic: syntax error"))])))

(define (query)
  (define firstline (line))
  (if (and (syntax? firstline)
           (next-token-eq? '>))
      ;; read 1 or more additional lines
      (let loop ([lines #`(#,firstline)])
        ;(eprintf "current lines: ~a~n" lines)
        (define levels (let consume-level ([lvls (list (level))])
                         ;(eprintf "current lvls: ~a~n" lvls)
                         (if (next-token-eq? '>)
                             (consume-level (cons (level) lvls))
                             lvls)))
        (define nextline (line))
        (cond
          [(and (syntax? nextline)
                (next-token-eq? '>))
           (loop #`(#,@lines #,@levels #,nextline))]
          [(syntax? nextline)
           #`(query #,@lines #,@levels #,nextline)]
          [else
           (parse-error "query: syntax error")
           ;; return what we have so far anyway
           #`(query #,@lines)]))
      ;; else return just one line or an error
      #`(query #,firstline)))

(define (named-query)
  (define tkn (pop-token))
  (cond
    [else
     (parse-error "named-query: syntax error")]))

(define (level)
  ;(eprintf "calling level~n")
  (define tkn (pop-token))
  (if (eq? (token-type tkn) '>)
      #'(level)
      (parse-error "expected '>'")))

(define (line)
  (define o (offset))
  (pop-token)
  (define typ (type))
  (pop-token)
  (define tst (test))
  (cond
    [(next-token-eq? 'HWS)
     (pop-token)
     (if (next-token-eq? 'EOL)
         (begin
           (pop-token)
           #`(line #,o #,typ #,tst))
         #`(line #,o #,typ #,tst #,(message)))]
    [else
     #`(line #,o #,typ #,tst)]))

(define (name-line)
  (define tkn (pop-token))
  (cond
    [else
     (parse-error "name-line: syntax error")]))

(define (name-type)
  (define tkn (pop-token))
  (cond
    [else
     (parse-error "name-type: syntax error")]))

(define (offset)
  (define tkn (pop-token))
  (cond
    [(token-eq? tkn 'INTEGER)
     #`(offset #,(token-val tkn))]
    [(token-eq? tkn '&)
     (push-token tkn)
     #`(offset #,(or (try-rule reloffset)
                     (try-rule relindoff)
                     (parse-error "offset: syntax error after '&'")))]

    [(token-eq? tkn '\()
     (push-token tkn)
     #`(offset #,(indoff))]
    [else
     (parse-error "offset: syntax error")]))

(define (reloffset)
  (unless (token-eq? (pop-token) '&)
    (parse-error "reloffset: missing '&'"))
  (define tkn (pop-token))
  (cond
    [(eq? (token-type tkn) 'INTEGER)
     #`(reloffset #,(token-val tkn))]
    [else
     (parse-error "reloffset: syntax error")]))

(define (relindoff)
  (unless (token-eq? (pop-token) '&)
    (parse-error "reloffset: missing '&'"))
  #`(relindoff #,(indoff)))

(define (indoff)
  (unless (token-eq? (pop-token) '\()
    (parse-error "indoff: missing '('"))
  (define tkn (pop-token))
  (define offset1
    (cond
      [(eq? (token-type tkn) 'INTEGER)
       #`#,(token-val tkn)]
      [(eq? (token-type tkn) '&)
       #`(#,(reloffset))]
      [else
       (parse-error "offset1: syntax error")]))

  (define sz
    (if (size-token? (peek-token))
        (size)
        #f))

  (define operator
    (if (size-op-token? (peek-token))
        (op)
        #f))

  (define displacement
    (if operator
        (disp)
        #f))
  
  ;; consume closing ')'
  (unless (token-eq? (pop-token) '\))
    (parse-error "indoff: missing ')'"))
  
  (cond
    [displacement
     #`(indoff #,offset1 #,sz #,operator #,displacement)]
    [sz
      #`(indoff #,offset1 #,sz)]
    [else
     #`(indoff #,offset1)]))

(define (size)
  (define tkn (pop-token))
  (case (token-type tkn)
    [(.B .b .C .c) #`(size (byte #,(token-val tkn)))]
    [(|,B| |,b| |,C| |,c|) #`(size (ubyte #,(token-val tkn)))]
    [(.s .h) #`(size (leshort #,(token-val tkn)))]
    [(|,s| |,h|) #`(size (uleshort #,(token-val tkn)))]
    [(.S .H) #`(size (beshort #,(token-val tkn)))]
    [(|,S| |,H|) #`(size (ubeshort #,(token-val tkn)))]
    [(.l) #`(size (lelong #,(token-val tkn)))]
    [(|,l|) #`(size (ulelong #,(token-val tkn)))]
    [(.L) #`(size (belong #,(token-val tkn)))]
    [(|,L|) #`(size (ubelong #,(token-val tkn)))]
    [(.m) #`(size (melong #,(token-val tkn)))]
    [(|,m|) #`(size (umelong #,(token-val tkn)))]
    [else
     (parse-error "size: syntax error")]))

(define (op)
  (define invert? (token-eq? (peek-token) '~))
  (when invert? (pop-token))
  (define op-tkn (pop-token))
  (if invert?
      #`(op (invert "~") #,(token-val op-tkn))
      #`(op #,(token-val op-tkn))))

(define (disp)
  (if (token-eq? (peek-token) '\()
      #`(disp #,(memvalue))
      (let ([tkn (pop-token)])
        (if (token-eq? tkn 'INTEGER)
            #`(disp #,(token-val tkn))
            (parse-error "disp: expected integer")))))

(define (memvalue)
  (unless (token-eq? (pop-token) '\()
    (parse-error "memvalue: missing '('"))

  (define value (pop-token))
  
  (unless (token-eq? (pop-token) '\))
    (parse-error "memvalue: missing '('"))

  #`#,value)

(define (type)
  (eprintf "type: ~n")
  (define tkn (pop-token))
  #`(type #,(token-val tkn)))

(define (test)
    (eprintf "test: ~n")
  (define tkn (pop-token))
  #`(test #,(token-val tkn)))

(define (message)
  (eprintf "message: ~n")
  (define tkn (pop-token))
  (cond
    [else
     (parse-error "message: syntax error")]))


;; test helpers
;; ------------
(define (test-parse rule str)
  (define next-token (make-tokenizer (open-input-string str)))
  (set! list-of-tokens (get-all-tokens next-token (next-token) '()))
  ;(display list-of-tokens)
  (rule))
