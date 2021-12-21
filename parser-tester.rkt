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
(require "parser-fast.rkt")

(require magic/output)
(set-magic-verbosity! 'debug)


(parse-to-datum
 (apply-tokenizer 
  make-tokenizer 
  ;"0	beshort	0x0206	ALAN game data\n>2	short	<10	version 2.6%d\n\n0	ubyte    >0	\n")
  ;"0           string  MZ\n>0x18       leshort <0x40   MZ executable (MS-DOS)\n>0x18       leshort >0x3f\n>>(0x3c.l)  string  PE\\0\\0  PE executable (MS-Windows)\n0	beshort	0x0206	ALAN game data\n")
  ;"(4.s)    leshort 0x014c  COFF executable (MS-DOS, DJGPP)\n")
  ;"(4.s*512) leshort !0x014c MZ executable (MS-DOS)\n")
  ;"&(0x3c.l)       leshort 0x14c   for Intel 80386\n")
  ;"3    regex/12/csl    =[0-9]{1,50}\\ [0-9]{1,50}	Netpbm image data\n")
  ;"0	regex/100l	\\^CFLAGS	makefile script text\n")
  ;"0	regex/100       [A-Z0-9]{4}.{14}$\n")
  ;"44	leqwdate	x	atime=%s\n")
  ;"10	beshort-8   16	passed\n")
  ;"10	ubyte	-35	byte at address 0xa: signed when decimal comparison\n")
  ;"(0x3c.l)  string  =PE\\0\\0  PE executable (MS-Windows)\n")
  ;"18	search/4261301	TRUEVISION-XFILE.\\0\n")
  ;"0	name	tga-image\n>2	ubyte		<34		Targa image data\n")
  ;"0    use    \\^tga-image\n")
  ;"(8.l)    string   	x	name=%s\n")
  ;"(4.l)	use		tiff_ifd\n")
  ;"0    default   x\n")
  ;"&-0x18	search/1024	A\\ \\tTRUEVISION-XFILE.\\0   matched truevision\n")
  ;"0	search/cwt/1024 	<head>		HTML document text\n")
  ;"0	string/b	=DDS Microsoft DirectDraw Surface (DDS):\n")
  ;"0	ubelong&0xffF8fe00	0x0a000000\n")
  ;"0    leshort   x    match=%d\n")
  ;"(2.S-2)   belong   !0x28632943\n")
  ;"18	search/4261301/s	TRUEVISION-XFILE.\0\n")
  ;"(&-4.B)     uleshort                0x01EF\n")
  ;"3	ubyte		x	%d-bit\n>68    clear\n>>0    clear    x\n")
  ;"3	ubyte		x	%d-bit\n")
  ;"0xD4		string	!\\x62\\x6D\\x66\\x01\\x00	Windows help annotation\n")
  ;"0x208		lestring16	DOCUMENT	\b: \"%.128s\"\n")
  ;"12  beshort >1\n")
  ;"2	pstring/HJ	x		\b, comment: \"%s\"\n")
  ;"(2.S+2)	use	jpeg_segment\n")
  ;"(&0x10,l+(-4)) 	string		PK\3\4 \b, ZIP self-extracting archive (Info-Zip)\n")
  ;"4	uleshort&0x4842			>0			support\n")
  ;"0       name   	msdos-driver	DOS executable\n")
  ;"&-4	    indirect/r	x	\\b with\n")
  ;"0x5	ubyte-1  <31\n")
  ;"8	ulelong/64 x	\b, %u files\n")
  ;"16			search/0x49AF/s	\\x6c\\x03\n")
  ;"0	string		Windows\\ Registry\\ Editor\\ \n")
  ;"1	regex/c		\\^([^\\xd>]*|.*\\.hlp)	MS Windows help file Content, based \"%s\"\n")
  ;"88	belong	& 16			\b, readable\n")
  ;"(16.l)	use		zipcd\n")
  ;"&0			use 		help-ver-date\n")
  ;"&1	search/b5	\x64\n")
  ;&(&-0x4)	lelong	  >0			SAMtools BCF (Binary Call Format)\n")
  "0	ustring		\xFD7zXZ\x00		XZ compressed data\n")
  ;"(0x0118-0x0FF60)    ulelong&0x80000007  !0x80000007\n")
  ;"0	regex	\\^[[:space:]]*class[[:space:]]+[[:digit:][:alpha:]:_]+[[:space:]]*\\\\{(.*[\\n]*)*\\\\}(;)?$		C++ source text\n")
)

#;
(parse
 #f
 (make-tokenizer
  (open-input-string "\n\n2	short	<10	version 2.6%d\n")))

