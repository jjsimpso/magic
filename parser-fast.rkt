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
(require racket/match)

(provide parse
         parse-to-datum)

;; Impelment a recursive descent parser

(define list-of-tokens '())

(struct exn:parse-error exn:fail (err-srcloc)
  #:property prop:exn:srclocs
  (lambda (a-struct)
    (match a-struct
      [(exn:parse-error msg marks err-srcloc)
       (list err-srcloc)])))

(define (make-parse-error message err-srcloc)
  (exn:parse-error message (current-continuation-marks) err-srcloc))

;; helper functions
;; ----------------

(define (peek-token)
  (and (not (null? list-of-tokens))
       (car list-of-tokens)))

(define (token-ref index)
  (list-ref list-of-tokens index))

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

(define (op-token? tkn)
  (case (token-type tkn)
    [(~ + - * / % & || ^) #t]
    [else #f]))

(define (strflag-token? tkn)
  (case (token-type tkn)
    [(b c C t T w W s) #t]
    [else #f]))

(define (regflag-token? tkn)
  (case (token-type tkn)
    [(c s l) #t]
    [else #f]))

(define (pstrflag-token? tkn)
  (case (token-type tkn)
    [(B H h L l) #t]
    [else #f]))

(define (compare-token? tkn)
  (case (token-type tkn)
    [(= ! < > & ^) #t]
    [else #f]))

;; discards vertical white space: comments or blank lines
(define (discard-vws)
  (let loop ([tkn (peek-token)])
    (when (or (token-eq? tkn 'COMMENT)
              (token-eq? tkn 'MIME)
              (token-eq? tkn 'EOL))
      (pop-token)
      (loop (peek-token)))))

(define (try-rule rule-func)
  (define saved-tokens-list list-of-tokens)
  (with-handlers ([exn:parse-error? (lambda (exn)
                                      ;; on error, restore token list and return false
                                      ;(eprintf "try-rule caught parse error: ~a~n" (exn-message exn))
                                      (set! list-of-tokens saved-tokens-list)
                                      #f)])
    (rule-func)))

(define (parse-error str [err-srcloc (srcloc #f #f #f #f #f)])
  ;(eprintf str)
  ;(eprintf "~n")
  (raise (make-parse-error str err-srcloc)))

;; read all the tokens from the function token-source and return a list of tokens
(define (get-all-tokens token-source next-token tokens)
  (if (eof-object? next-token)
      (reverse tokens)
      (get-all-tokens token-source (token-source) (cons next-token tokens))))


;; Interface functions
;; -------------------

;; token-source is a function that returns the next token or a list of tokens
(define (parse source-path token-source)
  (if (procedure? token-source)
      (set! list-of-tokens (get-all-tokens token-source (token-source) '()))
      (set! list-of-tokens token-source))
    (magic))

(define parse-to-datum
  (case-lambda
    [(token-source) (parse-to-datum #f token-source)]
    [(source-path token-source)
     (syntax->datum (parse source-path token-source))]))


;; Grammar Rules
;; -------------

;; originating rule
(define (magic)
  (let loop ([magic-stx #'(magic)])
    (cond
      [(not (peek-token))
       magic-stx]
      [(or (next-token-eq? 'EOL)
           (next-token-eq? 'COMMENT)
           (next-token-eq? 'MIME))
       (pop-token)
       (loop magic-stx)]
      [(token-eq? (token-ref 2) 'name)
       (define q (named-query))
       (if (syntax? q)
           (loop #`(#,@magic-stx #,q))
           (parse-error "magic: syntax error"))]
      [else
       (define q (query))
       (if (syntax? q)
           (loop #`(#,@magic-stx #,q))
           (parse-error "magic: syntax error"))])))

(define (query)
  (define firstline (line))
  (discard-vws)
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
        (discard-vws)
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
  (define firstline (name-line))
  (discard-vws)
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
        (discard-vws)
        (cond
          [(and (syntax? nextline)
                (next-token-eq? '>))
           (loop #`(#,@lines #,@levels #,nextline))]
          [(syntax? nextline)
           #`(named-query #,@lines #,@levels #,nextline)]
          [else
           (parse-error "named-query: syntax error")
           ;; return what we have so far anyway
           #`(named-query #,@lines)]))
      ;; else return just one line or an error
      #`(named-query #,firstline)))

(define (level)
  ;(eprintf "calling level~n")
  (define tkn (pop-token))
  (if (eq? (token-type tkn) '>)
      #'(level)
      (parse-error "expected '>'")))

(define (line)
  (define o (offset))
  (unless (token-eq? (pop-token) 'HWS)
    (parse-error "line: expected HWS after offset"))

  (if (next-token-eq? 'clear)
      ;; clear line
      (begin
        (if (and (pop-token)
                 (next-token-eq? 'HWS)
                 (pop-token))
            (let ([tst (test)])
              (unless (token-eq? (pop-token) 'EOL)
                (parse-error "clear-line: expected end-of-line"))
              #`(clear-line #,o "clear" #,tst))
            (begin
              (unless (token-eq? (pop-token) 'EOL)
                (parse-error "clear-line: expected end-of-line"))
              #`(clear-line #,o "clear"))))
      ;; standard line
      (let ([typ (type)])
        (unless (token-eq? (pop-token) 'HWS)
          (parse-error "line: expected HWS after type"))
        (let ([tst (test)])
          (cond
            [(next-token-eq? 'HWS)
             (pop-token)
             (if (next-token-eq? 'EOL)
                 (begin
                   (pop-token)
                   #`(line #,o #,typ #,tst))
                 (let ([msg (message)])
                   (if (next-token-eq? 'EOL)
                       (begin
                         (pop-token)
                         #`(line #,o #,typ #,tst #,msg))
                       (parse-error "line: expected end-of-line"))))]
            [(next-token-eq? 'EOL)
             (pop-token)
             #`(line #,o #,typ #,tst)]
            [else
             (parse-error "line: syntax error")])))))

(define (name-line)
  (define o (offset))
  (unless (token-eq? (pop-token) 'HWS)
    (parse-error "name-line: expected HWS after offset"))
  (define typ (name-type))
  (unless (token-eq? (pop-token) 'HWS)
    (parse-error "name-line: expected HWS after type"))
  (define name-tkn (pop-token))
  (unless (token-eq? name-tkn 'MAGIC-NAME)
    (parse-error "name-line: expected MAGIC NAME"))
  (cond
    [(next-token-eq? 'HWS)
     (pop-token)
     (if (next-token-eq? 'EOL)
         (begin
           (pop-token)
           #`(name-line #,o #,typ #,(token-val name-tkn)))
         (let ([msg (message)])
           (if (next-token-eq? 'EOL)
               (begin
                 (pop-token)
                 #`(name-line #,o #,typ #,(token-val name-tkn) #,msg))
               (parse-error "name-line: expected end-of-line"))))]
    [(next-token-eq? 'EOL)
     (pop-token)
     #`(name-line #,o #,typ #,(token-val name-tkn))]
    [else
     (parse-error "name-line: syntax error")]))

(define (name-type)
  (define tkn (pop-token))
  (if (token-eq? tkn 'name)
      #'(name-type "name")
      (parse-error "name-type: syntax error")))

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
      [(token-eq? tkn 'INTEGER)
       #`#,(token-val tkn)]
      [(token-eq? tkn '&)
       (push-token tkn)
       #`#,(reloffset)]
      [else
       (parse-error "offset1: syntax error")]))

  (define sz
    (if (size-token? (peek-token))
        (size)
        #f))

  (define operator
    (if (op-token? (peek-token))
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
  (define invert? (next-token-eq? '~))
  (when invert? (pop-token))
  (if (op-token? (peek-token))
      (let ([op-tkn (pop-token)])
        (if invert?
            #`(op (invert "~") #,(token-val op-tkn))
            #`(op #,(token-val op-tkn))))
      (parse-error "op: syntax error")))

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

  (define value-tkn (pop-token))
  (unless (token-eq? value-tkn 'INTEGER)
    (parse-error "memvalue: expected integer"))
  
  (unless (token-eq? (pop-token) '\))
    (parse-error "memvalue: missing '('"))

  #`(memvalue #,(token-val value-tkn)))

(define (type)
  (define (dispatch-type)
    ;(eprintf "dispatching on token ~a~n" (peek-token))
    (case (token-type (peek-token))
      [(u byte short beshort leshort long lelong belong melong quad lequad bequad float befloat lefloat double bedouble ledouble
          date bedate ledate medate ldate beldate leldate meldate qdate leqdate beqdate qldate leqldate beqldate qwdate leqwdate beqwdate)
       (numeric)]
      [(regex search string lestring16 bestring16 pstring)
       (strtype)]
      [(default) (default)]
      [(use) (use)]
      [(indirect) (indirect)]
      [else
       (parse-error "type: unknown type" (srcloc-token-srcloc (peek-token)))]))
  
  #`(type #,(dispatch-type)))

(define (numeric)
  (define unsigned? (next-token-eq? 'u))
  (when unsigned? (pop-token))
  (define tkn (pop-token))
  (define stx 
    (case (token-type tkn)
      [(byte short beshort leshort long lelong belong melong quad lequad bequad float befloat lefloat double bedouble ledouble
             date bedate ledate medate ldate beldate leldate meldate qdate leqdate beqdate qldate leqldate beqldate qwdate leqwdate beqwdate)
       (if unsigned?
           #`(numeric "u" #,(token-val tkn))
           #`(numeric #,(token-val tkn)))]
      [else
       (parse-error "numeric: syntax error")]))
  (define nummask? (op-token? (peek-token)))
  (if nummask?
      #`(#,@stx #,(nummask))
      stx))

(define (nummask)
  (define operator (op))
  (if (and operator
           (next-token-eq? 'INTEGER))
      #`(nummask #,operator #,(token-val (pop-token)))
      (parse-error "nummask: syntax error")))

(define (strtype)
  (cond
    [(next-token-eq? 'string)
     (string8)]
    [(next-token-eq? 'search)
     (search)]
    [(next-token-eq? 'regex) (regex)]
    [(or (next-token-eq? 'lestring16)
         (next-token-eq? 'bestring16))
     (string16)]
    [(next-token-eq? 'pstring)
     (pstring)]
    [else
      (parse-error "strtype: syntax error")]))

(define (string8)
  (define tkn (pop-token))
  (cond
    [(and (token-eq? tkn 'string)
          (next-token-eq? '/))
     (pop-token)
     #`(string8 "string" #,@(strflags))]
    [(token-eq? tkn 'string)
     #'(string8 "string")]
    [else
      (parse-error "string8: syntax error")]))

(define (strflags)
  (if (strflag-token? (peek-token))
      (let loop ([tkn (pop-token)]
                 [flags #'()])
        (cond
          [(or (token-eq? tkn 'HWS)
               ; add check for / token for now, but only needed by search type and may lead to poor error messages
               (token-eq? tkn '/))
           (push-token tkn)
           flags]
          [(strflag-token? tkn)
           (loop (pop-token)
                 #`(#,@flags (strflag #,(token-val tkn))))]
          [else
           (parse-error "string: invalid flag" (srcloc-token-srcloc tkn))]))
      (parse-error "string: missing or invalid flag" (srcloc-token-srcloc (peek-token)))))

(define (search)
  (if (token-eq? (pop-token) 'search)
      (if(and (next-token-eq? '/)
              (pop-token))
         (let ([tkn (pop-token)])
           (cond
             [(token-eq? tkn 'INTEGER)
              (define cnt (token-val tkn))
              (if (and (next-token-eq? '/)
                       (pop-token))
                  #`(search (srchcnt #,cnt) #,@(strflags))
                  #`(search (srchcnt #,cnt)))]
             [(strflag-token? tkn)
              (push-token tkn)
              (define flags (strflags))
              (if (and (next-token-eq? '/)
                       (pop-token))
                  (if (next-token-eq? 'INTEGER)
                      #`(search (srchcnt #,(token-val (pop-token))) #,@flags)
                      (parse-error "search: expected integer" (srcloc-token-srcloc (peek-token))))
                  #`(search #,@flags))]
             [else
              (parse-error "search: expected flag or count" (srcloc-token-srcloc tkn))]))
         #'(search))
      (parse-error "search: expected 'search'")))

(define (regex)
  (if (token-eq? (pop-token) 'regex)
      (let ([tkn (pop-token)])
        (cond
          [(and (token-eq? tkn '/)
                (next-token-eq? 'INTEGER))
           (let ([cnt (token-val (pop-token))])
             (cond
               [(and (next-token-eq? '/)
                     (pop-token))
                #`(regex (regcnt #,cnt) #,@(regflags))]
               [(regflag-token? (peek-token))
                #`(regex (regcnt #,cnt) #,@(regflags))]
               [else
                #`(regex (regcnt #,cnt))]))]
          [(token-eq? tkn '/)
           #`(regex #,@(regflags))]
          [(token-eq? tkn 'HWS)
           (push-token tkn)
           #`(regex)]
          [else
           (parse-error "regex: syntax error" (srcloc-token-srcloc tkn))]))
      (parse-error "regex: expected 'regex'")))

(define (regflags)
  (if (regflag-token? (peek-token))
      (let loop ([tkn (pop-token)]
                 [flags #'()])
        (cond
          [(token-eq? tkn 'HWS)
           (push-token tkn)
           flags]
          [(regflag-token? tkn)
           ;(eprintf "regflags: got ~a~n" (token-val tkn))
           (loop (pop-token)
                 #`(#,@flags (regflag #,(token-val tkn))))]
          [else
           (parse-error "regex: invalid flag" (srcloc-token-srcloc tkn))]))
      (parse-error "regex: missing or invalid flag" (srcloc-token-srcloc (peek-token)))))

(define (string16)
  (define tkn (pop-token))
  (if (or (token-eq? tkn 'bestring16)
          (token-eq? tkn 'lestring16))
      #`(string16 #,(token-val tkn))
      (parse-error "string16: expected 'bestring16' or 'lestring16'" (srcloc-token-srcloc tkn))))

(define (pstring)
  (if (token-eq? (pop-token) 'pstring)
      (if (and (next-token-eq? '/)
               (pop-token))
         (let ([tkn (pop-token)])
           (cond
             [(token-eq? tkn 'J)
              #`(pstring "pstring" (pstrjflag "J"))]
             [(pstrflag-token? tkn)
              (cond
                [(next-token-eq? 'J)
                 (pop-token)
                 #`(pstring "pstring" (pstrflag #,(token-val tkn)) (pstrjflag "J"))]
                [(next-token-eq? 'HWS)
                 #`(pstring "pstring" (pstrflag #,(token-val tkn)))]
                [else
                 (parse-error "pstring: only 'J' is allowed for second flag" (srcloc-token-srcloc (peek-token)))])]
             [else
              (parse-error "pstring: expected flag" (srcloc-token-srcloc tkn))]))
         #'(pstring "pstring"))
      (parse-error "pstring: expected 'pstring'")))

(define (default)
  (define tkn (pop-token))
  (if (token-eq? tkn 'default)
      #'(default "default")
      (parse-error "default: syntax error" (srcloc-token-srcloc tkn))))

(define (use)
  (define tkn (pop-token))
  (if (token-eq? tkn 'use)
      #'(use "use")
      (parse-error "use: syntax error" (srcloc-token-srcloc tkn))))

(define (indirect)
  (define tkn (pop-token))
  (cond
    [(and (token-eq? tkn 'indirect)
          (next-token-eq? '/))
     (pop-token)
     (if (token-eq? (pop-token) 'r)
         #'(indirect "indirect" "r")
         (parse-error "indirect: expected 'r'" (srcloc-token-srcloc tkn)))]
    [(token-eq? tkn 'indirect)
     #'(indirect "indirect")]
    [else
      (parse-error "indirect: syntax error" (srcloc-token-srcloc tkn))]))

(define (test)
  #`(test #,(or (try-rule numtest)
                (try-rule strtest)
                (try-rule truetest)
                (try-rule use-name)
                (parse-error "test: syntax error"))))

(define (numtest)
  (define tkn (pop-token))
  (cond
    [(token-eq? tkn 'INTEGER)
     #`(numtest #,(token-val tkn))]
    [(and (compare-token? tkn)
          (next-token-eq? 'INTEGER))
     #`(numtest #,(token-val tkn) #,(token-val (pop-token)))]
    [else
     (parse-error "numtest: syntax error")]))

(define (strtest)
  (define tkn (pop-token))
  (cond
    [(token-eq? tkn 'STRING)
     #`(strtest #,(token-val tkn))]
    [(and (compare-token? tkn)
          (next-token-eq? 'STRING))
     #`(strtest #,(token-val tkn) #,(token-val (pop-token)))]
    [else
     (parse-error "strtest: syntax error")]))

;; currently unused. rolled into numtest and strtest
(define (compare)
  (define tkn (pop-token))
  (if (compare-token? tkn)
      #`#,(token-val tkn)
      (parse-error "compare: syntax error")))

(define (truetest)
  (define tkn (pop-token))
  (if (token-eq? tkn 'x)
      #'(truetest "x")
      (parse-error "truetest: expected 'x'")))

(define (use-name)
  (define tkn (pop-token))
  (if (token-eq? tkn 'MAGIC-NAME)
      #`(use-name #,(token-val tkn))
      (parse-error "use-name: expected MAGIC-NAME token")))

(define (message)
  (define tkn (pop-token))
  (cond
    [(token-eq? tkn 'STRING)
     #`(message #,(token-val tkn))]
    [else
     (parse-error "message: syntax error")]))

;; test helpers
;; ------------
#;(define (test-parse rule str)
  (define next-token (make-test-tokenizer (open-input-string str)))
  (set! list-of-tokens (get-all-tokens next-token (next-token) '()))
  ;(display list-of-tokens)
  (rule))
