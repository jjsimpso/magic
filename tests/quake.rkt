#lang magic

# Update: Joerg Jenderek
# URL: http://fileformats.archiveteam.org/wiki/PAK
# reference: https://quakewiki.org/wiki/.pak
# GRR: line below is too general as it matches also Acorn PackDir compressed Archive
# and Git pack ./revision
0       string  PACK    PAK file, 
# real Quake examples like pak0.pak have only some hundreds like 150 files
# So test for few files
>8	ulelong <0x01000000	
# in file version 5.32 test for null terminator is only true for
# offset ~< FILE_BYTES_MAX = 1 MB defined in ../../src/file.h 
# look for null terminator of 1st entry name
>>(4.l+55)	ubyte	0	Quake I or II world or extension
>>>8	ulelong/64 x	\b, %u files
# offset to the beginning of the file table
>>>4	ulelong	x	\b, offset 0x%x
# 1st file entry
>>>(4.l)	use	pak-entry
# 2nd file entry
#>>>4	ulelong+64	x	\b, offset 0x%x
#>>>(4.l+64)	use	pak-entry
#
#	display file table entry of Quake PAK archive
0	name		pak-entry
# normally entry start after header which implies offset 12 or higher
>56	ulelong	>11	
# the offset from the beginning of pak to beginning of this entry file contents
>>56	ulelong	x	at 0x%x
# the size of file for this entry 
>>60	ulelong	x	%u bytes
# 56 byte null-terminated entry name string includes path like maps/e1m1.bsp
>>0	string	x	'%-.56s'
# inspect entry content by jumping to entry offset
>>(56)	indirect x	\b: 
