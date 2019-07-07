#lang racket

(require "signed-magic.rkt")
(with-input-from-file "../file-tests/test.bin" magic-query)
