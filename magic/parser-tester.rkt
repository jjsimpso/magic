#lang racket

(require brag/support)

(require "reader.rkt")
(require "parser.rkt")

(parse-to-datum
 (apply-tokenizer 
  make-tokenizer 
  ;"0	beshort	0x0206	ALAN game data\n>2	short	<10	version 2.6%d\n\n0	ubyte    >0	\n")
  "0           string  MZ\n>0x18       leshort <0x40   MZ executable (MS-DOS)\n>0x18       leshort >0x3f\n>>(0x3c.l)  string  PE\\0\\0  PE executable (MS-Windows)\n")
)
;(parse-to-datum "2	short	<10	version 2.6%d")
