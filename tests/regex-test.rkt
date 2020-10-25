#lang magic

# Workaround for Targa image type by Joerg Jenderek
# GRR: line below too general as it catches also
# Targa image type 1 with 26 long identification field
# and HELP.DSK
0	string		\032\001
# 5th character of terminal name list, but not Targa image pixel size (15 16 24 32)
>16	ubyte		>32
# namelist, if more than 1 separated by "|" like "st|stterm| simpleterm 0.4.1"
>>12	regex		\^[a-zA-Z0-9][a-zA-Z0-9.][^|]*	Compiled terminfo entry "%-s"
