#lang racket

;;Copyright 2019 Jonathan Simpson
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.

(require (only-in "file-exe.rkt" (magic-query exe-query) (magic-query-run-all exe-query-all)))
(require (only-in "debugtest.rkt" (magic-query image-query)))
(require (only-in "string-test.rkt" (magic-query string-query)))

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

(with-input-from-file "sundesk2.gif" image-query)
(with-input-from-file "pantherxl.jpg" image-query)
(with-input-from-file "Pureyouth.jpg" image-query)
(with-input-from-file "Pureyouth.jpg" (build-string-output-thunk image-query))
(with-input-from-file "TOWN.PCX" image-query)
(with-input-from-file "gs3armor.htm" image-query)

(with-input-from-file "iexplore.exe" exe-query-all)
