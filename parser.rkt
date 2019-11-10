#lang brag

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

magic : EOL* (query | named-query)+
query : line (level+ line)* /EOL*
level : /">"
line : offset /HWS type /HWS test (/HWS message?)? /EOL

named-query : name-line (level+ line)* /EOL*
name-line : offset /HWS name-type /HWS MAGIC-NAME /EOL

offset : absoffset | reloffset | indoffset
@absoffset : INTEGER     ; An absolute offset from the start of the file.
reloffset : /"&" INTEGER ; The offset relative to the last match offset one level up. Not allowed at level == 0
@indoffset : indoff | relindoff 
indoff : /"(" offset1 [ size ] [ op disp ] /")" ;; Read the file at <offset1> of width <size>.
;; If size is not specified, assume a long.
;; If <op> is given, then preform that
;; operation on the result and the <disp>.
@offset1 : absoffset | reloffset
size : byte | leshort | beshort | lelong | belong | melong

byte : ".B" | ".b" | ".C" | ".c"	; A one-byte value.
leshort : ".s" | ".h"		; A two-byte little-endian value.
beshort : ".S" | ".H"		; A two-byte big-endian value.
lelong : ".l"			; A four-byte little-endian value.
belong : ".L"			; A four-byte big-endian value.
melong : ".m"			; A four-byte middle-endian value.

op : [ invert ] ( "+" | "-" | "*" | "/" | "%" | "&" | "|" | "^" )

invert : "~"		; Flip the bits on result of the <op>.

disp :  INTEGER | memvalue

memvalue : "(" INTEGER ")" 
;; INTEGER is interpreted as an absolute or
;; relative offset matching that of <offset1>.
;; Read the file at the resulting offset with
;; the same size as <offset1>
relindoff : /"&" indoff	; add <indoff> to the last match offset at one level up

type : ( numeric | strtype | default | use)

@unsigned : "u"
;; The value is unsigned.
;; This affects the sign extension of numeric
;; types and the '<' and '>' compares.  It is
;; intended for numeric types, but allowed on
;; all types.
numeric : [ unsigned ] ( numtype | datetype ) [ nummask ]

@numtype : byt | short | long | quad
@byt : "byte"
@short : "short" | "beshort" | "leshort"
@long : "long" | "lelong" | "belong" | "melong"
@quad : "quad" | "lequad" | "bequad"

datetype : udate32 | ldate32 | udate64 | ldate64
udate32 : "date" | "bedate" | "ledate" | "medate"	;; UTC dates
ldate32 : "ldate" | "beldate" | "leldate" | "meldate"	;; local dates
udate64 : "qdate" | "leqdate" | "beqdate"		;; UTC dates
ldate64 : "qldate" | "leqldate" | "beqldate"		;; local dates

nummask : op INTEGER

@strtype : regex | search | string8 | string16

regex : "regex" [ "/" regflag* ]
regflag : "c" | "s" | linecnt
linecnt : INTEGER     ; The number of lines to search.  If this missing or zero, the rest of the file is searched.

search : /"search" [ /"/" srchflag ] [ /"/" srchflag ]
@srchflag : strflag+ | srchcnt
srchcnt : INTEGER     ; The number of search tries.  If this is missing or zero, the rest of the file is searched.

string8 : ( "string" | "pstring" ) [ /"/" strflag+ ]
strflag : "b" | "c" | "C" | "t" | "T" | "w" | "W" | "s"
string16 : "bestring16" | "lestring16"
use : "use"
default : "default"
;; This is intended to be used with the
;; <truetest> ("x" below).  It is matched if
;; there has been no previous match at its
;; level or none since the last default at
;; that level.  It is useful for implementing
;; switch-like and if/else constructions.

test : numtest | strtest | truetest | use-name  ; Test to preform on <type> read from file.
numtest : [ compare ] INTEGER	      ; If compare is missing, "=" is assumed.
strtest : [ compare ] STRING	
;; If compare is missing, "=" is assumed.
;; Note: If the STRING begins with a <compare>
;; character, the <compare> field cannot be
;; omitted.
@compare : "=" | "!" | "<" | ">" | "&" | "^"
truetest : "x"	    ; This always returns true. To test for the string "x" use "=x".
use-name : MAGIC-NAME

message : [ nospflag ] ( STRING | FMT_STRING )  ; Message to print if test result is true.
nospflag : "%x08" | "\b"	  ; Do not insert a space before the message. By default, messages are separated by a " ".

name-type : "name"

;HWS : " " | "\t"
;EOL : "\n"
;FMTSTR : STRING
;INTEGER : ("0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9")+
;STRING : ("A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" | "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z")+
;FMTSTR : <printf format string with exactly one % construct>
