#lang magic

0	string/b	DDS Microsoft DirectDraw Surface (DDS):
0	string/W	test\ compacted\ whitespace	W flag passed
0	string/cCW	testing\ case\ insensitive	cC flag passed

# Type: Nintendo 3DS "SMDH" file. (application description)
# From: David Korth <gerbilsoft@gerbilsoft.com>
# Reference: https://3dbrew.org/wiki/SMDH
0		string		SMDH		Nintendo 3DS SMDH file
>0x208		leshort		!0
>>0x208		lestring16	x		\b: "%.128s"
>>0x388		leshort		!0
>>>0x388	lestring16	x		by %.128s
>0x208		leshort		0
>>0x008		leshort		!0
>>>0x008	lestring16	x		\b: "%.128s"
>>>0x188	leshort		!0
>>>>0x188	lestring16	x		by %.128s

0x442	lestring16	C:\\DOCUME      Symbian .sis file
