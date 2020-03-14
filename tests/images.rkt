#lang magic

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


# Targa - matches `povray', `ppmtotga' and `xv' outputs
# by Philippe De Muyter <phdm@macqel.be>
# URL: http://justsolve.archiveteam.org/wiki/TGA
# Reference: http://www.dca.fee.unicamp.br/~martino/disciplinas/ea978/tgaffs.pdf
# Update: Joerg Jenderek
# at 2, byte ImgType must be 1, 2, 3, 9, 10 or 11
#	,32 or 33 (both not observed)
# at 1, byte CoMapType must be 1 if ImgType is 1 or 9, 0 otherwise
#	or theoretically 2-128 reserved for use by Truevision or 128-255 may be used for developer applications
# at 3, leshort Index is 0 for povray, ppmtotga and xv outputs
# `xv' recognizes only a subset of the following (RGB with pixelsize = 24)
# `tgatoppm' recognizes a superset (Index may be anything)
#
# test of Color Map Type 0~no 1~color map
# and Image Type 1 2 3 9 10 11 32 33
# and Color Map Entry Size 0 15 16 24 32
0	ubequad&0x00FeC400000000C0	0
# Prevent conflicts with CRI ADX.
>(2.S-2) belong	!0x28632943
# skip more garbage like *.iso by looking for positive image type
>>2	ubyte			>0
# skip some compiled terminfo like xterm+tmux by looking for image type less equal 33
>>>2	ubyte			<34
# skip arches.3200 , Finder.Root , Slp.1 by looking for low pixel depth 1 8 15 16 24 32
>>>>16	ubyte			1
>>>>>0		use		tga-image
>>>>16	ubyte			8
>>>>>0		use		tga-image
>>>>16	ubyte			15
>>>>>0		use		tga-image
>>>>16	ubyte			16
>>>>>0		use		tga-image
>>>>16	ubyte			24
>>>>>0		use		tga-image
>>>>16	ubyte			32
>>>>>0		use		tga-image
#	display tga bitmap image information
0	name				tga-image
>2	ubyte		<34		Targa image data
!:mime	image/x-tga
!:apple	????TPIC
# normal extension .tga but some Truevision products used others:
# tpic (Apple),icb (Image Capture Board),vda (Video Display Adapter),vst (NuVista),win (UNSURE about that)
!:ext	tga/tpic/icb/vda/vst
# image type 1 2 3 9 10 11 32 33
>2	ubyte&0xF7	1		- Map
>2	ubyte&0xF7	2		- RGB
# alpha channel
>>17	ubyte&0x0F	>0		\bA
>2	ubyte&0xF7	3		- Mono
# type not found, but by http://www.fileformat.info/format/tga/corion.htm
# Compressed color-mapped data, using Huffman, Delta, and runlength encoding
>2	ubyte		32		- Color
# Compressed color-mapped data, using Huffman, Delta, and RLE. 4-pass quadtree- type process
>2	ubyte		33		- Color
# Color Map Type 0~no 1~color map
>1	ubyte		1		(
# first color map entry, 0 normal
>>3	uleshort	>0		\b%d-
# color map length 0 2 1dh 3bh d9h 100h
>>5	uleshort	x		\b%d)
# 8~run length encoding bit
>2	ubyte&0x08	8		- RLE
# gimp can create big pictures!
>12	uleshort	>0		%d x
>12	uleshort	=0		65536 x
# image height. 0 interpreted as 65536
>14	uleshort	>0		%d
>14	uleshort	=0		65536
# Image Pixel depth 1 8 15 16 24 32
>16	ubyte		x		x %d
# X origin of image. 0 normal
>8	uleshort	>0		+%d
# Y origin of image. 0 normal; positive for top
>10	uleshort	>0		+%d
# Image descriptor: bits 3-0 give the alpha channel depth, bits 5-4 give direction
>17	ubyte&0x0F	>0		- %d-bit alpha
# bits 5-4 give direction. normal bottom left
>17	ubyte		&0x20		- top
#>17	ubyte		^0x20		- bottom
>17	ubyte		&0x10		- right
#>17	ubyte		^0x10		- left
# some info say other bits 6-7 should be zero
# but data storage interleave by http://www.fileformat.info/format/tga/corion.htm
# 00 - no interleave;01 - even/odd interleave; 10 - four way interleave; 11 - reserved
#>17	ubyte&0xC0	0x00		- no interleave
>17	ubyte&0xC0	0x40		- interleave
>17	ubyte&0xC0	0x80		- four way interleave
>17	ubyte&0xC0	0xC0		- reserved
# positive length implies identification field
>0	ubyte		>0
>>18	string		x		"%s"
# last 18 bytes of newer tga file footer signature
>18	search/4261301/s	TRUEVISION-XFILE.\0
# extension area offset if not 0
>>&-8		ulelong			>0
# length of the extension area. normal 495 for version 2.0
>>>(&-4.l)	uleshort		0x01EF
# AuthorName[41]
>>>>&0		string			>\0		- author "%-.40s"
# Comment[324]=4 * 80 null terminated
>>>>&41		string			>\0		- comment "%-.80s"
# date
>>>>&365	ubequad&0xffffFFFFffff0000	!0
# Day
>>>>>&-6		uleshort		x		%d
# Month
>>>>>&-8		uleshort		x		\b-%d
# Year
>>>>>&-4		uleshort		x		\b-%d
# time
>>>>&371	ubequad&0xffffFFFFffff0000	!0
# hour
>>>>>&-8		uleshort		x		%d
# minutes
>>>>>&-6		uleshort		x		\b:%.2d
# second
>>>>>&-4		uleshort		x		\b:%.2d
# JobName[41]
>>>>&377		string			>\0		- job "%-.40s"
# JobHour Jobminute Jobsecond
>>>>&418	ubequad&0xffffFFFFffff0000	!0
>>>>>&-8		uleshort		x		%d
>>>>>&-6		uleshort		x		\b:%.2d
>>>>>&-4		uleshort		x		\b:%.2d
# SoftwareId[41]
>>>>&424		string			>\0		- %-.40s
# SoftwareVersionNumber
>>>>&424	ubyte				>0
>>>>>&40		uleshort/100		x		%d
>>>>>&40		uleshort%100		x		\b.%d
# VersionLetter
>>>>>&42		ubyte			>0x20		\b%c
# KeyColor
>>>>&468		ulelong			>0		- keycolor 0x%8.8x
# Denominator of Pixel ratio. 0~no pixel aspect
>>>>&474	uleshort			>0
# Numerator
>>>>>&-4		uleshort		>0		- aspect %d
>>>>>&-2		uleshort		x		\b/%d
# Denominator of Gamma ratio. 0~no Gamma value
>>>>&478	uleshort			>0
# Numerator
>>>>>&-4		uleshort		>0		- gamma %d
>>>>>&-2		uleshort		x		\b/%d
# ColorOffset
#>>>>&480	ulelong			x		- col offset 0x%8.8x
# StampOffset
#>>>>&484	ulelong			x		- stamp offset 0x%8.8x
# ScanOffset
#>>>>&488	ulelong			x		- scan offset 0x%8.8x
# AttributesType
#>>>>&492	ubyte			x		- Attributes 0x%x
## EndOfTGA

# Tag Image File Format, from Daniel Quinlan (quinlan@yggdrasil.com)
# The second word of TIFF files is the TIFF version number, 42, which has
# never changed.  The TIFF specification recommends testing for it.
0	string		MM\x00\x2a	TIFF image data, big-endian
!:strength +70
!:mime	image/tiff
>(4.L)	use		^tiff_ifd
0	string		II\x2a\x00	TIFF image data, little-endian
!:mime	image/tiff
!:strength +70
>(4.l)	use		tiff_ifd

0	name		tiff_ifd
>0	leshort		x		\b, direntries=%d
>2	use		tiff_entry

0	name		tiff_entry
# NewSubFileType
>0	leshort		0xfe
>>12	use		tiff_entry
>0	leshort		0x100
>>4	lelong		1
>>>12	use		tiff_entry
>>>8	leshort		x		\b, width=%d
>0	leshort		0x101
>>4	lelong		1
>>>8	leshort		x		\b, height=%d
>>>12	use		tiff_entry
>0	leshort		0x102
>>8	leshort		x		\b, bps=%d
>>12	use		tiff_entry
>0	leshort		0x103
>>4	lelong		1		\b, compression=
>>>8	leshort		1		\bnone
>>>8	leshort		2		\bhuffman
>>>8	leshort		3		\bbi-level group 3
>>>8	leshort		4		\bbi-level group 4
>>>8	leshort		5		\bLZW
>>>8	leshort		6		\bJPEG (old)
>>>8	leshort		7		\bJPEG
>>>8	leshort		8		\bdeflate
>>>8	leshort		9		\bJBIG, ITU-T T.85
>>>8	leshort		0xa		\bJBIG, ITU-T T.43
>>>8	leshort		0x7ffe		\bNeXT RLE 2-bit
>>>8	leshort		0x8005		\bPackBits (Macintosh RLE)
>>>8	leshort		0x8029		\bThunderscan RLE
>>>8	leshort		0x807f		\bRasterPadding (CT or MP)
>>>8	leshort		0x8080		\bRLE (Line Work)
>>>8	leshort		0x8081		\bRLE (High-Res Cont-Tone)
>>>8	leshort		0x8082		\bRLE (Binary Line Work)
>>>8	leshort		0x80b2		\bDeflate (PKZIP)
>>>8	leshort		0x80b3		\bKodak DCS
>>>8	leshort		0x8765		\bJBIG
>>>8	leshort		0x8798		\bJPEG2000
>>>8	leshort		0x8799		\bNikon NEF Compressed
>>>8	default		x
>>>>8	leshort		x		\b(unknown 0x%x)
>>>12	use		tiff_entry
>0	leshort		0x106		\b, PhotometricIntepretation=
>>8	clear
>>8	leshort		0		\bWhiteIsZero
>>8	leshort		1		\bBlackIsZero
>>8	leshort		2		\bRGB
>>8	leshort		3		\bRGB Palette
>>8	leshort		4		\bTransparency Mask
>>8	leshort		5		\bCMYK
>>8	leshort		6		\bYCbCr
>>8	leshort		8		\bCIELab
>>8	default		x
>>>8	leshort		x		\b(unknown=0x%x)
>>12	use		tiff_entry
# FillOrder
>0	leshort		0x10a
>>4	lelong		1
>>>12	use		tiff_entry
# DocumentName
>0	leshort		0x10d
>>(8.l)	string		x		\b, name=%s
>>>12	use		tiff_entry
# ImageDescription
>0	leshort		0x10e
>>(8.l)	string		x		\b, description=%s
>>>12	use		tiff_entry
# Make
>0	leshort		0x10f
>>(8.l)	string		x		\b, manufacturer=%s
>>>12	use		tiff_entry
# Model
>0	leshort		0x110
>>(8.l)	string		x		\b, model=%s
>>>12	use		tiff_entry
# StripOffsets
>0	leshort		0x111
>>12	use		tiff_entry
# Orientation
>0	leshort		0x112		\b, orientation=
>>8	leshort		1		\bupper-left
>>8	leshort		3		\blower-right
>>8	leshort		6		\bupper-right
>>8	leshort		8		\blower-left
>>8	leshort		9		\bundefined
>>8	default		x
>>>8	leshort		x		\b[*%d*]
>>12	use		tiff_entry
# XResolution
>0	leshort		0x11a
>>8	lelong		x		\b, xresolution=%d
>>12	use		tiff_entry
# YResolution
>0	leshort		0x11b
>>8	lelong		x		\b, yresolution=%d
>>12	use		tiff_entry
# ResolutionUnit
>0	leshort		0x128
>>8	leshort		x		\b, resolutionunit=%d
>>12	use		tiff_entry
# Software
>0	leshort		0x131
>>(8.l)	string		x		\b, software=%s
>>12	use		tiff_entry
# Datetime
>0	leshort		0x132
>>(8.l)	string		x		\b, datetime=%s
>>12	use		tiff_entry
# HostComputer
>0	leshort		0x13c
>>(8.l)	string		x		\b, hostcomputer=%s
>>12	use		tiff_entry
# WhitePoint
>0	leshort		0x13e
>>12	use		tiff_entry
# PrimaryChromaticities
>0	leshort		0x13f
>>12	use		tiff_entry
# YCbCrCoefficients
>0	leshort		0x211
>>12	use		tiff_entry
# YCbCrPositioning
>0	leshort		0x213
>>12	use		tiff_entry
# ReferenceBlackWhite
>0	leshort		0x214
>>12	use		tiff_entry
# Copyright
>0	leshort		0x8298
>>(8.l)	string		x		\b, copyright=%s
>>12	use		tiff_entry
# ExifOffset
>0	leshort		0x8769
>>12	use		tiff_entry
# GPS IFD
>0	leshort		0x8825		\b, GPS-Data
>>12	use		tiff_entry

#>0	leshort		x		\b, unknown=0x%x
#>>12	use		tiff_entry

0	string		MM\x00\x2b	Big TIFF image data, big-endian
!:mime	image/tiff
0	string		II\x2b\x00	Big TIFF image data, little-endian
!:mime	image/tiff
