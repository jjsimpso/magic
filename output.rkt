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

(define (print-fatal form . v)
  (when (>= verbosity 1) (print-msg form v)))

(define (print-error form . v)
  (when (>= verbosity 2) (print-msg form v)))

(define (print-warning form . v)
  (when (>= verbosity 3) (print-msg form v)))

(define (print-notice form . v)
  (when (>= verbosity 4) (print-msg form v)))

(define (print-info form . v)
  (when (>= verbosity 5) (print-msg form v)))

(define (print-debug form . v)
  (when (>= verbosity 6) (print-msg form v)))

(define (print-msg form v)
  (if (null? v)
      (printf form)
      (apply eprintf (cons form v))))
