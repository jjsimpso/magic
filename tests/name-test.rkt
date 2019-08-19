#lang magic

#	display tga bitmap image information
0	name				tga-image
>2	byte		<34		Targa image data
#>0      use             tga-image

0	name				test-scope
>2	byte		<34		Test Scope
>0      use             tga-image

#0      use             tga-image
