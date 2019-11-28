#lang magic

# test the offset after search is set to end of matched string
0	search/4096/cwt	  \<html\>		HTML document text
>&0     string            \<BODY                gs3armor.htm

# test that the search flag "s" leaves offset at the beginning of the match
0       search/s          bc                    bc
>&0     string            bcdef                 \bdef

# test indirect relative offsets
0          ubelong           0x502319ae            test.bin
# the following line tests that an end of file error doesn't print anything that is treatead as part
# of the result
>(&2.l-50) leshort           24912                 will trigger error
>(&2.b-50) leshort           24912                 determined with indirect relative offset test

# test octal escapes
0	string/b	\46\046\11\61\062\63\12 octal test passed

# test string compare operator
0       string          >mnww        string greater than passed
