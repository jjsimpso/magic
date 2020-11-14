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
