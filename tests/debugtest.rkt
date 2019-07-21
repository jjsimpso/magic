#lang magic

0	string/b	DDS\040\174\000\000\000 Microsoft DirectDraw Surface (DDS):

# PCX image files
# From: Dan Fandrich <dan@coneharvesters.com>
# updated by Joerg Jenderek at Feb 2013 by http://de.wikipedia.org/wiki/PCX
# http://web.archive.org/web/20100206055706/http://www.qzx.com/pc-gpe/pcx.txt
# GRR: original test was still too general as it catches xbase examples T5.DBT,T6.DBT with 0xa000000
# test for bytes 0x0a,version byte (0,2,3,4,5),compression byte flag(0,1), bit depth (>0) of PCX or T5.DBT,T6.DBT
0	ubelong&0xffF8fe00	0x0a000000
# for PCX bit depth > 0
>3	ubyte		>0
# test for valid versions
>>1	ubyte		<6
>>>1	ubyte		!1	PCX
>>>>1	ubyte		0	ver. 2.5 image data
>>>>1	ubyte		2	ver. 2.8 image data, with palette
>>>>1	ubyte		3	ver. 2.8 image data, without palette
>>>>1	ubyte		4	for Windows image data
>>>>1	ubyte		5	ver. 3.0 image data
>>>>4	uleshort	x	bounding box [%d,
>>>>6	uleshort	x	%d] -
>>>>8	uleshort	x	[%d,
>>>>10	uleshort	x	%d],
>>>>65	ubyte		>1	%d planes each of
>>>>3	ubyte		x	%d-bit
>>>>68	byte		1	colour,
>>>>68	byte		2	grayscale,
# this should not happen
>>>>68	default		x	image,
>>>>12	leshort		>0	%d x
>>>>>14	uleshort	x	%d dpi,
>>>>2	byte		0	uncompressed
>>>>2	byte		1	RLE compressed


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

# GIF
# Strength set up to beat 0x55AA DOS/MBR signature word lookups (+65)
0	string		GIF8		GIF image data
!:strength +80
!:mime	image/gif
!:apple	8BIMGIFf
>4	string		7a		\b, version 8%s,
>4	string		9a		\b, version 8%s,
>6	leshort		>0		%d x
>8	leshort		>0		%d
#>10	byte		&0x80		color mapped,
#>10	byte&0x07	=0x00		2 colors
#>10	byte&0x07	=0x01		4 colors
#>10	byte&0x07	=0x02		8 colors
#>10	byte&0x07	=0x03		16 colors
#>10	byte&0x07	=0x04		32 colors
#>10	byte&0x07	=0x05		64 colors
#>10	byte&0x07	=0x06		128 colors
#>10	byte&0x07	=0x07		256 colors



0	beshort		0xffd8		JPEG image data
!:mime	image/jpeg
!:apple	8BIMJPEG
!:strength *3
!:ext jpeg/jpg/jpe/jfif
>6	string		JFIF		\b, JFIF standard
>6	string		Exif		\b, Exif standard: [
>>12	default		x		\b]
