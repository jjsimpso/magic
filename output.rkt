#lang racket/base

(provide set-magic-verbosity!)
(provide print-fatal print-error print-warning print-notice print-info print-debug)

(define verbosity 2)

(define (set-magic-verbosity! value)
  (cond
    [(eq? value 'none) (set! verbosity 0)]
    [(eq? value 'fatal) (set! verbosity 1)]
    [(eq? value 'error) (set! verbosity 2)]
    [(eq? value 'warning) (set! verbosity 3)]
    [(eq? value 'notice) (set! verbosity 4)]
    [(eq? value 'info) (set! verbosity 5)]
    [(eq? value 'debug) (set! verbosity 6)]))

(define-syntax-rule (print-fatal form v ...)
  (when (>= verbosity 1) (eprintf form v ...)))

(define-syntax-rule (print-error form v ...)
  (when (>= verbosity 2) (eprintf form v ...)))

(define-syntax-rule (print-warning form v ...)
  (when (>= verbosity 3) (eprintf form v ...)))

(define-syntax-rule (print-notice form v ...)
  (when (>= verbosity 4) (eprintf form v ...)))

(define-syntax-rule (print-info form v ...)
  (when (>= verbosity 5) (eprintf form v ...)))

(define-syntax-rule (print-debug form v ...)
  (when (>= verbosity 6) (eprintf form v ...)))
