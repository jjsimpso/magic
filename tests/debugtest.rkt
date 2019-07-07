#lang magic

0	string/b	DDS\040\174\000\000\000 Microsoft DirectDraw Surface (DDS):

#
# 137 P N G \r \n ^Z \n [4-byte length] I H D R [HEAD data] [HEAD crc] ...
#

# IHDR parser
0	name		png-ihdr
>0	belong		x		\b, %d x
>4	belong		x		%d,
>8	byte		x		%d-bit
>9	byte		0		grayscale,
>9	byte		2		\b/color RGB,
>9	byte		3		colormap,
>9	byte		4		gray+alpha,
>9	byte		6		\b/color RGBA,
#>10	byte		0		deflate/32K,
>12	byte		0		non-interlaced
>12	byte		1		interlaced

# Standard PNG image.
0	string		\x89PNG\x0d\x0a\x1a\x0a\x00\x00\x00\x0DIHDR	PNG image data
>16	use		png-ihdr
