#lang racket

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

(require "reader.rkt")
(require "parser.rkt")

(parse-to-datum
 (apply-tokenizer 
  make-tokenizer 
  ;"0	beshort	0x0206	ALAN game data\n>2	short	<10	version 2.6%d\n\n0	ubyte    >0	\n")
  ;"0           string  MZ\n>0x18       leshort <0x40   MZ executable (MS-DOS)\n>0x18       leshort >0x3f\n>>(0x3c.l)  string  PE\\0\\0  PE executable (MS-Windows)\n0	beshort	0x0206	ALAN game data\n")
  ;"(4.s*512)    leshort 0x014c  COFF executable (MS-DOS, DJGPP)\n")
  ;"(4.s*512) leshort !0x014c MZ executable (MS-DOS)\n")
  ;"&(0x3c.l)       leshort 0x14c   for Intel 80386\n")
  ;"10	ubyte	-35	byte at address 0xa: signed when decimal comparison\n")
  ;"(0x3c.l)  string  =PE\\0\\0  PE executable (MS-Windows)\n")
  ;"18	search/4261301	TRUEVISION-XFILE.\\0\n")
  ;"0	name	tga-image\n>2	ubyte		<34		Targa image data\n")
  ;"0    use    \\^tga-image\n")
  ;"(4.l)	use		tiff_ifd\n")
  ;"0    default   x\n")
  ;"&18	search/1024	A\\ \\tTRUEVISION-XFILE.\\0   matched truevision\n")
  ;"0	search/cwt/1024 	<head>		HTML document text\n")
  ;"0	string/b	=DDS Microsoft DirectDraw Surface (DDS):\n")
  ;"0	ubelong&0xffF8fe00	0x0a000000\n")
  ;"(2.S-2)   belong   !0x28632943\n")
  ;"18	search/4261301/s	TRUEVISION-XFILE.\0\n")
  ;"(&-4.B)     uleshort                0x01EF\n")
  "3	ubyte		x	%d-bit\n>68    clear\n>>0    clear    x\n")
  ;"3	ubyte		x	%d-bit\n")
)
;(parse-to-datum "2	short	<10	version 2.6%d")
