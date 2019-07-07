#lang racket/base
;(module reader reprovide magic/reader)

(module reader racket/base
  (provide read-syntax)
  (require magic/reader))
