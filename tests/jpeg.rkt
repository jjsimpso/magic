#lang magic

#------------------------------------------------------------------------------
# $File: jpeg,v 1.31 2017/03/17 21:35:28 christos Exp $
# JPEG images
# SunOS 5.5.1 had
#
#	0	string		\377\330\377\340	JPEG file
#	0	string		\377\330\377\356	JPG file
#
# both of which turn into "JPEG image data" here.
#
0	beshort		0xffd8		JPEG image data
!:mime	image/jpeg
!:apple	8BIMJPEG
!:strength *3
!:ext jpeg/jpg/jpe/jfif
>6	string		JFIF		\b, JFIF standard
# The following added by Erik Rossen <rossen@freesurf.ch> 1999-09-06
# in a vain attempt to add image size reporting for JFIF.  Note that these
# tests are not fool-proof since some perfectly valid JPEGs are currently
# impossible to specify in magic(4) format.
# First, a little JFIF version info:
>>11	byte		x		\b %d.
>>12	byte		x		\b%02d
# Next, the resolution or aspect ratio of the image:
>>13	byte		0		\b, aspect ratio
>>13	byte		1		\b, resolution (DPI)
>>13	byte		2		\b, resolution (DPCM)
>>14	beshort		x		\b, density %dx
>>16	beshort		x		\b%d
>>4	beshort		x		\b, segment length %d
# Next, show thumbnail info, if it exists:
>>18	byte		!0		\b, thumbnail %dx
>>>19	byte		x		\b%d
>6	string		Exif		\b, Exif standard: [
#>>12	indirect/r	x
>>12	string		x		\b]

# Jump to the first segment
>(4.S+4)	use		jpeg_segment

# This uses recursion...
0		name		jpeg_segment
>0	beshort		0xFFFE
# Recursion handled by FFE0
#>>(2.S+2)	use			jpeg_segment
>>2	pstring/HJ	x		\b, comment: "%s"

>0	beshort		0xFFC0
>>(2.S+2)	use			jpeg_segment
>>4	byte		x		\b, baseline, precision %d
>>7	beshort		x		\b, %dx
>>5	beshort		x		\b%d
>>9	byte		x		\b, frames %d

>0	beshort		0xFFC1
>>(2.S+2)	use			jpeg_segment
>>4	byte		x		\b, extended sequential, precision %d
>>7	beshort		x		\b, %dx
>>5	beshort		x		\b%d
>>9	byte		x		\b, frames %d

>0	beshort		0xFFC2
>>(2.S+2)	use			jpeg_segment
>>4	byte		x		\b, progressive, precision %d
>>7	beshort		x		\b, %dx
>>5	beshort		x		\b%d
>>9	byte		x		\b, frames %d

# Define Huffman Tables
>0	beshort		0xFFC4
>>(2.S+2)	use			jpeg_segment

>0	beshort		0xFFE1
# Recursion handled by FFE0
#>>(2.S+2)	use			jpeg_segment
>>4	string		Exif		\b, Exif Standard: [
#>>>10	indirect/r	x
>>>10	string		x		\b]

# Application specific markers
>0	beshort&0xFFE0	=0xFFE0
>>(2.S+2)	use			jpeg_segment

# DB: Define Quantization tables
# DD: Define Restart interval [XXX: wrong here, it is 4 bytes]
# D8: Start of image
# D9: End of image
# Dn: Restart
>0	beshort&0xFFD0	=0xFFD0
>>0	beshort&0xFFE0	!0xFFE0
>>>(2.S+2)	use			jpeg_segment

#>0	beshort		x		unknown 0x%x
#>>(2.S+2)	use			jpeg_segment

# HSI is Handmade Software's proprietary JPEG encoding scheme
0	string		hsi1		JPEG image data, HSI proprietary

# From: David Santinoli <david@santinoli.com>
0	string		\x00\x00\x00\x0C\x6A\x50\x20\x20\x0D\x0A\x87\x0A	JPEG 2000
# From: Johan van der Knijff <johan.vanderknijff@kb.nl>
# Added sub-entries for JP2, JPX, JPM and MJ2 formats; added mimetypes
# https://github.com/bitsgalore/jp2kMagic
#
# Now read value of 'Brand' field, which yields a few possibilities:
>20	string		\x6a\x70\x32\x20	Part 1 (JP2)
!:mime	image/jp2
>20	string		\x6a\x70\x78\x20	Part 2 (JPX)
!:mime	image/jpx
>20	string		\x6a\x70\x6d\x20	Part 6 (JPM)
!:mime	image/jpm
>20	string		\x6d\x6a\x70\x32	Part 3 (MJ2)
!:mime	video/mj2

# Type: JPEG 2000 codesream
# From: Mathieu Malaterre <mathieu.malaterre@gmail.com>
0	belong		0xff4fff51						JPEG 2000 codestream
45	beshort		0xff52

# JPEG extended range
0	string		\x49\x49\xbc
>3	byte		1
>>4	lelong%2	0	JPEG-XR
!:mime	image/jxr
!:ext	jxr
