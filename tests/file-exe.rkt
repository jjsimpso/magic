#lang magic

# MS Windows executables are also valid MS-DOS executables
0           string  MZ
>0x18       leshort <0x40
>>(4.s*512) leshort 0x014c  COFF executable (MS-DOS, DJGPP)
>>(4.s*512) leshort !0x014c MZ executable (MS-DOS)
>>>&(2.s-514)	string	BW
>>>>0x240	search/0x100	DOS/4G	\b, LE for MS-DOS, DOS4GW DOS extender (embedded)

# skip the whole block below if it is not an extended executable
>0x18       leshort >0x3f
>>(0x3c.l)  string  PE\0\0  PE executable (MS-Windows)
>>>&0       leshort 0x14c   for Intel 80386
>>>&0       leshort 0x184   for DEC Alpha
>>>&0       leshort 0x8664  for AMD64
# search for the PE section called ".idata"...
>>>&0xf4    search/0x140 .idata
# ...and go to the end of it, calculated from start+length;
# these are located 14 and 10 bytes after the section name
>>>>(&0xe.l+(-4)) string       PK\3\4 \b, ZIP self-extracting archive
>>(0x3c.l)  string  LX\0\0  LX executable (OS/2)

0                string  MZ
>0x18            leshort >0x3f
>>(0x3c.l)       string  LE\0\0 LE executable (MS-Windows)
# at offset 0x58 inside the LE header, we find the relative offset
# to a data area where we look for a specific signature
#>>>&(&0x54.l-3)  string  UNACE  \b, ACE self-extracting archive
