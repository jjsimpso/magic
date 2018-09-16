#lang racket

(require brag/support)

(require "reader.rkt")
(require "parser.rkt")

(parse-to-datum
 (apply-tokenizer 
  make-tokenizer 
  "0	beshort	0x0206	ALAN game data\n>2	short	<10	version 2.6%d\n\n0	ubyte    >0	test\n")
)
;(parse-to-datum "2	short	<10	version 2.6%d")
