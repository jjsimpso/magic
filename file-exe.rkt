#lang reader "reader.rkt"

# MS Windows executables are also valid MS-DOS executables
0           string  MZ
>0x18       leshort <0x40
>>(4.s*512) leshort 0x014c  COFF executable (MS-DOS, DJGPP)
>>(4.s*512) leshort !0x014c MZ executable (MS-DOS)
# skip the whole block below if it is not an extended executable
>0x18       leshort >0x3f
>>(0x3c.l)  string  PE\0\0  PE executable (MS-Windows) 
>>>&0       leshort 0x14c   for Intel 80386
>>>&0       leshort 0x184   for DEC Alpha
>>>&0       leshort 0x8664  for AMD64
>>(0x3c.l)  string  LX\0\0  LX executable (OS/2)

0	beshort	0x0206	ALAN game data

#0	string	t\ \t\    test string escapes

0	string	MZ 
>0x2    string  aaa       testing

#	display tga bitmap image information
0	name				tga-image
>2	byte		<34		Targa image data
#>2	ubyte		<34		Targa image data

0       name    always-true
>0      default x  force true

0       use     always-true
