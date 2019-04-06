#lang racket

(require brag/support)

(require "reader.rkt")
(require "parser.rkt")

(parse-to-datum
 (apply-tokenizer 
  make-tokenizer 
  "0	beshort	0x0206	ALAN game data\n>2	short	<10	version 2.6%d\n\n0	ubyte    >0	\n")
  ;"0           string  MZ\n>0x18       leshort <0x40   MZ executable (MS-DOS)\n>0x18       leshort >0x3f\n>>(0x3c.l)  string  PE\\0\\0  PE executable (MS-Windows)\n0	beshort	0x0206	ALAN game data\n")
  ;"(4.s*512)    leshort 0x014c  COFF executable (MS-DOS, DJGPP)\n")
  ;"(4.s*512) leshort !0x014c MZ executable (MS-DOS)\n")
  ;"&(0x3c.l)       leshort 0x14c   for Intel 80386\n")
  ;"(0x3c.l)  string  PE\\0\\0  PE executable (MS-Windows)\n")
  ;"18	search/4261301	TRUEVISION-XFILE.\\0\n")
)
;(parse-to-datum "2	short	<10	version 2.6%d")