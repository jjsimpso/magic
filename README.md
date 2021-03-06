#lang magic
===========

#lang magic is a Racket implementation of the mini language used by the Unix file command. This is a language designed to write tests to determine a file's type. Thousands of tests written in the magic language can be seamlessly incorporated into Racket applications using #lang magic. In addition, compared to the file command, Racket provides a secure environment for executing these tests.

## Requirements
* Racket version 7.4.0.902 or higher. 
* The brag Racket package. Install with `raco pkg install brag`.
* #lang magic is currently being developed and tested with Racket on Linux. It has also been tested briefly on Windows and didn't exhibit any obvious problems. I will occassionaly spot check the Windows version, but I'm not currently testing every commit against Windows.

## Description 
For the curious, The man pages for 'magic' describes the magic language in considerable, but not exhaustive, detail. A code sample to check for Microsoft executables provides the flavor of the language:

```
# MS Windows executables are also valid MS-DOS executables
0           string  MZ
>0x18       leshort <0x40
>>(4.s*512) leshort 0x014c  COFF executable (MS-DOS, DJGPP)
>>(4.s*512) leshort !0x014c MZ executable (MS-DOS)
# skip the whole block below if it is not an extended executable
>0x18       leshort >0x3f
>>(0x3c.l)  string  PE\0\0  PE executable (MS-Windows)
>>>&0       leshort 0x14c   for Intel 80386
>>>&0       leshort 0x184   for DEC Alpha
>>>&0       leshort 0x8664  for AMD64
>>(0x3c.l)  string  LX\0\0  LX executable (OS/2)
```

In #lang magic the code sample above compiles to one magic query. A #lang magic Racket module consists of 1 or more such queries. New queries start on a line without a preceding '>'. Every #lang magic module provides two functions: magic-query and magic-query-run-all. These functions are thunks that can be passed to with-input-from-file to test the file against the queries in the module. magic-query replicates the default behavior of the file command. It stops and returns true after the first query match or returns false if no queries pass. A query is matched if the test on the first line of the query succeeds. A matched query will run until completion, but even if later tests in the query fail, the query is still considered a match if the first test passes. magic-query-run-all, on the other hand, will always test the file against every query in the module. Both functions print the messages for each successful test to the current output port.

This is a brief summary, so consult 'man magic' for a complete explanation of magic syntax. Windows users can google the Linux man page for 'magic'. The tests/ directory also contains sample magic files and calls to them from test.rkt.

## Status 
There is still a lot of work to do. Some of the macros need revising and should probably be rewritten with syntax/parse. The lexer could be improved as well. And of course there are missing features, some of which are documented in the TODO file.

## Goals 
The goal is to get as close to 100% compatibility as possible with Ian Darwin's version(available at https://www.darwinsys.com/file/) found in most Linux and BSD distributions.
