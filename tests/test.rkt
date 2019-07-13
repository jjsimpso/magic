#lang racket

(require (rename-in "file-exe.rkt" (magic-query exe-query)))
(require (rename-in "debugtest.rkt" (magic-query image-query)))
(require (rename-in "string-test.rkt" (magic-query string-query)))

(define (build-string-output-thunk thnk)
  (lambda ()
    (let ([retval (with-output-to-string thnk)])
      (if (non-empty-string? retval)
          retval
          #f))))

(with-input-from-file "iexplore.exe" exe-query)
(with-input-from-file "iexplore.exe" (build-string-output-thunk exe-query))
(with-input-from-file "mm3.exe" exe-query)
(with-input-from-file "mm3.exe" (build-string-output-thunk exe-query))
;(with-input-from-file "SIMTOWER.EXE" exe-query)
(with-input-from-file "/home/jonathan/.bash_history" exe-query)
(with-input-from-file "/home/jonathan/.bash_history" (build-string-output-thunk exe-query))

(with-input-from-file "../file-tests/W-flag-test.txt" string-query)
(with-input-from-file "../file-tests/W-flag-test.txt" (build-string-output-thunk string-query))
(with-input-from-file "../file-tests/cC-flag-test.txt" string-query)
(with-input-from-file "../file-tests/cC-flag-test.txt" (build-string-output-thunk string-query))

(with-input-from-file "small_avatar.png" image-query)
(with-input-from-file "small_avatar.png" (build-string-output-thunk image-query))
(with-input-from-file "thg2.png" image-query)
(with-input-from-file "thg2.png" (build-string-output-thunk image-query))
