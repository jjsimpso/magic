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
>>12	string		x		\b]
