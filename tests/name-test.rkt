#lang magic

#
0	name				check-mz
>0	string	MZ                      MZ match

0	name				test-scope
>0      use                             check-mz

0      use             test-scope
