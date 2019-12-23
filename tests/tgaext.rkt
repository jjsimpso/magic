#lang magic

# Targa extension block for printf substitution testing
# length of the extension area. normal 495 for version 2.0
0	uleshort		0x01EF
# AuthorName[41]
>&0		string			>\0		- author "%-.40s"
# Comment[324]=4 * 80 null terminated
>&41		string			>\0		- comment "%-.50s"
# date
>&365	ubequad&0xffffFFFFffff0000	!0
# Day
>>&-6		uleshort		x		%d
# Month
>>&-8		uleshort		x		\b-%d
# Year
>>&-4		uleshort		x		\b-%d
# time
>&371	ubequad&0xffffFFFFffff0000	!0
# hour
>>&-8		uleshort		x		%d
# minutes
>>&-6		uleshort		x		\b:%.2d
# second
>>&-4		uleshort		x		\b:%.2d
# JobName[41]
>&377		string			>\0		- job "%-.40s"
# JobHour Jobminute Jobsecond
>&418	ubequad&0xffffFFFFffff0000	!0
>>&-8		uleshort		x		%d
>>&-6		uleshort		x		\b:%.2d
>>&-4		uleshort		x		\b:%.2d
# SoftwareId[41]
>&424		string			>\0		- %-.40s
# SoftwareVersionNumber
>&424	ubyte				>0
>>&40		uleshort/100		x		%d
>>&40		uleshort%100		x		\b.%d
# VersionLetter
>>&42		ubyte			>0x20		\b%c
# KeyColor
>&468		ulelong			>0		- keycolor 0x%8.8x
# Denominator of Pixel ratio. 0~no pixel aspect
>&474	uleshort			>0
# Numerator
>>&-4		uleshort		>0		- aspect %d
>>&-2		uleshort		x		\b/%d
# Denominator of Gamma ratio. 0~no Gamma value
>&478	uleshort			>0
# Numerator
>>&-4		uleshort		>0		- gamma %d
>>&-2		uleshort		x		\b/%d
# ColorOffset
#>&480	ulelong			x		- col offset 0x%8.8x
# StampOffset
#>&484	ulelong			x		- stamp offset 0x%8.8x
# ScanOffset
#>&488	ulelong			x		- scan offset 0x%8.8x
# AttributesType
#>&492	ubyte			x		- Attributes 0x%x
