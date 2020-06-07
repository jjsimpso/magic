#lang magic

#
0	name				check-mz
>0	string  MZ                      MZ match,

0	name				test-scope
>0      use                             check-mz

0       name                            void-name

0       name                            leshort-test
>2      leshort 192                     leshort match,
#>2      leshort 0x00c0                  leshort match,

0       name                            uleshort-test
>2      uleshort <193                   uleshort match,

0       name                            beshort-test
>2      beshort 0xc000                  beshort match,
>2      beshort =-16384                  beshort neg. match,

0       name                            nummask-test
>2      ubelong&0xff000000  0xc0000000  nummask match

# it appears that using a name from the top-level doesn't work properly (it never matches)
# file appears to behave the same way, but I don't remember doing this intentionaly in #lang magic
# this is logical though, since a use line always matches and you wouldn't want that at the top-level
0	string	MZ
>0      use             void-name
>0      use             ^void-name
>0      use             test-scope
>0      use             leshort-test
>0      use             ^uleshort-test
>0      use             ^beshort-test
>0      use             nummask-test
