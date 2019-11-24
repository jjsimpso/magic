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

(require rackunit)

(require (only-in "file-exe.rkt" (magic-query exe-query) (magic-query-run-all exe-query-all)))
(require (only-in "images.rkt" (magic-query image-query)))
(require (only-in "debugtest.rkt" (magic-query debugtest-query)))
(require (only-in "string-test.rkt" (magic-query string-query)))

(define (build-string-output-thunk thnk)
  (lambda ()
    (let ([retval (with-output-to-string thnk)])
      (if (non-empty-string? retval)
          retval
          #f))))

;; exe tests
;; ------------------------------------------------------------------
(with-input-from-file "iexplore.exe" exe-query)
(check-equal? 
 (with-input-from-file "iexplore.exe" (build-string-output-thunk exe-query)) 
 "PE executable (MS-Windows) for AMD64 ")

(with-input-from-file "mm3.exe" exe-query)
(check-equal? 
 (with-input-from-file "mm3.exe" (build-string-output-thunk exe-query)) 
 "MZ executable (MS-DOS) ")

;(with-input-from-file "SIMTOWER.EXE" exe-query)

(with-input-from-file "/home/jonathan/.bash_history" exe-query)
(check-equal? 
 (with-input-from-file "/home/jonathan/.bash_history" (build-string-output-thunk exe-query)) 
 #f)

;; string tests
;; ------------------------------------------------------------------
(with-input-from-file "../file-tests/W-flag-test.txt" string-query)
(check-equal? 
 (with-input-from-file "../file-tests/W-flag-test.txt" (build-string-output-thunk string-query)) 
 "W flag passed ")
(with-input-from-file "../file-tests/cC-flag-test.txt" string-query)
(check-equal? 
 (with-input-from-file "../file-tests/cC-flag-test.txt" (build-string-output-thunk string-query)) 
 "cC flag passed ")

;; image tests
;; ------------------------------------------------------------------
(with-input-from-file "small_avatar.png" image-query)
(check-equal? 
 (with-input-from-file "small_avatar.png" (build-string-output-thunk image-query)) 
 "PNG image data \b, 94 x 159, 8-bit colormap, non-interlaced ")

(with-input-from-file "thg2.png" image-query)
(check-equal? 
 (with-input-from-file "thg2.png" (build-string-output-thunk image-query)) 
 "PNG image data \b, 1282 x 476, 8-bit \b/color RGB, non-interlaced ")

(with-input-from-file "sundesk2.gif" image-query)
(check-equal? 
 (with-input-from-file "sundesk2.gif" (build-string-output-thunk image-query))
 "GIF image data \b, version 8%s, 1152 x 900 ")

(with-input-from-file "pantherxl.jpg" image-query)
(check-equal? 
 (with-input-from-file "pantherxl.jpg" (build-string-output-thunk image-query))
 "JPEG image data \b, JFIF standard ")

(with-input-from-file "Pureyouth.jpg" image-query)
(check-equal? 
 (with-input-from-file "Pureyouth.jpg" (build-string-output-thunk image-query))
 "JPEG image data \b, Exif standard: [ \b] ")

(with-input-from-file "TOWN.PCX" image-query)
(check-equal? 
 (with-input-from-file "TOWN.PCX" (build-string-output-thunk image-query))
 "PCX ver. 3.0 image data bounding box [0, 0] - [639, 679], 1-bit colour, image, 150 x 150 dpi, RLE compressed ")

(with-input-from-file "03.tga" image-query)
(check-equal? 
 (with-input-from-file "03.tga" (build-string-output-thunk image-query))
 "Targa image data - RGB - RLE 500 x 387 x 24 ")


;; debug tests
;; ------------------------------------------------------------------
(with-input-from-file "gs3armor.htm" debugtest-query)
(check-equal? 
(with-input-from-file "gs3armor.htm" (build-string-output-thunk debugtest-query))
 "HTML document text gs3armor.htm ")

(with-input-from-file "../file-tests/abcdef" debugtest-query)
(check-equal? 
(with-input-from-file "../file-tests/abcdef" (build-string-output-thunk debugtest-query))
 "bc \bdef ")

(with-input-from-file "../file-tests/test.bin" debugtest-query)
(check-equal? 
(with-input-from-file "../file-tests/test.bin" (build-string-output-thunk debugtest-query))
 "test.bin determined with indirect relative offset test ")

(with-input-from-file "../file-tests/octal-test.txt" debugtest-query)
(check-equal? 
(with-input-from-file "../file-tests/octal-test.txt" (build-string-output-thunk debugtest-query))
 "octal test passed ")


(with-input-from-file "iexplore.exe" exe-query-all)
(check-equal? 
(with-input-from-file "iexplore.exe" (build-string-output-thunk exe-query-all))
 "PE executable (MS-Windows) for AMD64 *** *** ")
