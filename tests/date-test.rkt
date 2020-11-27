#lang magic

0	string		\114\0\0\0\001\024\002\0\0\0\0\0\300\0\0\0\0\0\0\106	MS Windows shortcut
>20	lelong&1	1	\b, Item id list present
>20	lelong&2	2	\b, Points to a file or directory
>20	lelong&4	4	\b, Has Description string
>20	lelong&8	8	\b, Has Relative path
>20	lelong&16	16	\b, Has Working directory
>20	lelong&32	32	\b, Has command line arguments
>20	lelong&64	64	\b, Icon
>>56	lelong		x	\b number=%d
>24	lelong&1	1	\b, Read-Only
>24	lelong&2	2	\b, Hidden
>24	lelong&4	4	\b, System
>24	lelong&8	8	\b, Volume Label
>24	lelong&16	16	\b, Directory
>24	lelong&32	32	\b, Archive
>24	lelong&64	64	\b, Encrypted
>24	lelong&128	128	\b, Normal
>24	lelong&256	256	\b, Temporary
>24	lelong&512	512	\b, Sparse
>24	lelong&1024	1024	\b, Reparse point
>24	lelong&2048	2048	\b, Compressed
>24	lelong&4096	4096	\b, Offline
>28	leqwdate	x	\b, ctime=%s
>36	leqwdate	x	\b, mtime=%s
>44	leqwdate	x	\b, atime=%s
>52	lelong		x	\b, length=%u, window=
>60	lelong&1	1	\bhide
>60	lelong&2	2	\bnormal
>60	lelong&4	4	\bshowminimized
>60	lelong&8	8	\bshowmaximized
>60	lelong&16	16	\bshownoactivate
>60	lelong&32	32	\bminimize
>60	lelong&64	64	\bshowminnoactive
>60	lelong&128	128	\bshowna
>60	lelong&256	256	\brestore
>60	lelong&512	512	\bshowdefault
#>20	lelong&1	0
#>>20	lelong&2	2
#>>>(72.l-64)	pstring/h	x	\b [%s]
#>20	lelong&1	1
#>>20	lelong&2	2
#>>>(72.s)	leshort	x
#>>>&75	pstring/h	x	\b [%s]


# check and then display version and date inside MS Windows HeLP file fragment
0	name				help-ver-date
# look for Magic of SYSTEMHEADER
>0	leshort		0x036C
# version Major		1 for right file fragment
>>4	leshort		1		Windows
>>>2	leshort		0x0F		3.x
>>>2	leshort		0x15		3.0
>>>2	leshort		0x21		3.1
>>>2	leshort		0x27		x.y
>>>2	leshort		0x33		95
>>>2	default		x		y.z
>>>>2	leshort		x		0x%x
>>>2	leshort		x		help
>>>6	ldate		x		\b, %s

# Magic for HeLP files
0	lelong		0x00035f3f
# ./windows (version 5.25) labeled the entry as "MS Windows 3.x help file"
# file header magic 0x293B at DirectoryStart+9
>(4.l+9)	uleshort	0x293B		MS
# look for @VERSION	bmf.. like IBMAVW.ANN
>>0xD4		string	=\x62\x6D\x66\x01\x00	Windows help annotation
!:mime	application/x-winhelp
!:ext	ann
>>0xD4		string	!\x62\x6D\x66\x01\x00
# "GID Help index" by TrID
>>>(4.l+0x65)	string	=|Pete			Windows help Global Index
!:mime	application/x-winhelp
!:ext	gid
# HeLP Bookmark or
# "Windows HELP File" by TrID
>>>(4.l+0x65)		string		!|Pete
# maybe there exist a cleaner way to detect HeLP fragments
# brute search for Magic 0x036C with matching Major maximal 7 iterations
# discapp.hlp
>>>>16			search/0x49AF/s	\x6c\x03
>>>>>&0			use 		help-ver-date
>>>>>&4			leshort		!1
# putty.hlp
>>>>>>&0		search/0x69AF/s	\x6c\x03
>>>>>>>&0		use 		help-ver-date
