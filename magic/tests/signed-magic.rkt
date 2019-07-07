#lang magic

#0	leshort	0x2350

#10	leshort	0xa5dd	short at address 0xa: unsigned when hex comparison,
#>10	leshort	42461 	unsigned when decimal comparison,
#>10	leshort	-23075	signed when decimal comparison,
#>10	uleshort 	42461	unsigned when unsigned decimal comparison,
#>10	uleshort 	-23075	signed when unsigned decimal comparison,

10	byte	-35	byte at address 0xa: signed when decimal comparison,
>10	ubyte	-35	signed when unsigned decimal comparison
>10	byte	0xdd	unsigned when hex comparison,
>10	byte	221	unsigned when decimal comparison,
>10	ubyte	0xdd	unsigned when unsigned hex comparison,
>10	ubyte	221	unsigned when unsigned decimal comparison,
>10	byte	<-34	signed < -34,
>10	ubyte	<-34	unsigned < -34,
>10	byte	>-36	signed > -36,
>10	ubyte	>-36	unsigned > -36,
