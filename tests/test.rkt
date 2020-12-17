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
(require (only-in "tgaext.rkt" (magic-query tgaext-query)))
(require (only-in "name-test.rkt" (magic-query name-query)))
(require (only-in "regex-test.rkt" (magic-query regex-query)))
(require (only-in "date-test.rkt" (magic-query date-query)))
(require (only-in "jpeg.rkt" (magic-query jpeg-query)))
;; just get the magic-result structure definitions once
(require (except-in "jpeg.rkt" magic-query magic-query-run-all))

(require magic/output)
(set-magic-verbosity! 'warning)

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
 (magic-result-output-text (with-input-from-file "iexplore.exe" exe-query))
 "PE executable (MS-Windows) for AMD64 ")

(with-input-from-file "mm3.exe" exe-query)
(check-equal? 
 (magic-result-output-text (with-input-from-file "mm3.exe" exe-query))
 "MZ executable (MS-DOS) ")

(with-input-from-file "roc.png" exe-query)
(check-equal? 
 (with-input-from-file "roc.png" exe-query)
 #f)

;; string tests
;; ------------------------------------------------------------------
(with-input-from-file "../file-tests/W-flag-test.txt" string-query)
(check-equal? 
 (magic-result-output-text (with-input-from-file "../file-tests/W-flag-test.txt" string-query))
 "W flag passed ")
(with-input-from-file "../file-tests/cC-flag-test.txt" string-query)
(check-equal? 
 (magic-result-output-text (with-input-from-file "../file-tests/cC-flag-test.txt" string-query)) 
 "cC flag passed ")

;; image tests
;; ------------------------------------------------------------------
(with-input-from-file "wizardry.png" image-query)
(check-equal? 
 (magic-result-output-text (with-input-from-file "wizardry.png" image-query))
 "PNG image data \b, 320 x 200, 8-bit colormap, non-interlaced ")

(with-input-from-file "roc.png" image-query)
(check-equal? 
 (magic-result-output-text (with-input-from-file "roc.png" image-query))
 "PNG image data \b, 407 x 177, 8-bit \b/color RGBA, non-interlaced ")

(with-input-from-file "sundesk2.gif" image-query)
(check-equal? 
 (magic-result-output-text (with-input-from-file "sundesk2.gif" image-query))
 "GIF image data \b, version 87a, 1152 x 900 ")

(with-input-from-file "pantherxl.jpg" image-query)
(check-equal? 
 (magic-result-output-text (with-input-from-file "pantherxl.jpg" image-query))
 "JPEG image data \b, JFIF standard ")

(with-input-from-file "Pureyouth.jpg" image-query)
(check-equal? 
 (magic-result-output-text (with-input-from-file "Pureyouth.jpg" image-query))
 "JPEG image data \b, Exif standard: [ \b] ")

(with-input-from-file "TOWN.PCX" image-query)
(check-equal? 
 (magic-result-output-text (with-input-from-file "TOWN.PCX" image-query))
 "PCX ver. 3.0 image data bounding box [0, 0] - [639, 679], 1-bit colour, 150 x 150 dpi, RLE compressed ")

(with-input-from-file "03.tga" image-query)
(check-equal? 
 (magic-result-output-text (with-input-from-file "03.tga" image-query))
 "Targa image data - RGB - RLE 500 x 387 x 24 ")

(with-input-from-file "palmpic2.tif" image-query)
(check-equal? 
 (magic-result-output-text (with-input-from-file "palmpic2.tif" image-query))
 "TIFF image data, little-endian \b, direntries=16 \b, height=223 \b, bps=206 \b, compression= \bnone \b, PhotometricIntepretation= \bRGB \b, width=164 ")

(with-input-from-file "le.tif" image-query)
(check-equal? 
 (magic-result-output-text (with-input-from-file "le.tif" image-query))
 "TIFF image data, little-endian \b, direntries=21 \b, height=223 \b, bps=44462 \b, compression= \bnone \b, PhotometricIntepretation= \bRGB \b, name=le.tiff \b, orientation= \bupper-left \b, width=164 ")

(with-input-from-file "be.tif" image-query)
(check-equal? 
 (magic-result-output-text (with-input-from-file "be.tif" image-query))
 "TIFF image data, big-endian \b, direntries=21 \b, height=223 \b, bps=1 \b, compression= \bnone \b, PhotometricIntepretation= \bRGB \b, name=be.tiff \b, orientation= \bupper-left \b, width=164 ")


;; debug tests
;; ------------------------------------------------------------------
(with-input-from-file "gs3armor.htm" debugtest-query)
(check-equal? 
 (magic-result-output-text (with-input-from-file "gs3armor.htm" debugtest-query))
 "HTML document text gs3armor.htm ")

(with-input-from-file "FAQ-by--odino.txt" debugtest-query)
(check-equal? 
 (magic-result-output-text (with-input-from-file "FAQ-by--odino.txt" debugtest-query))
 "Found FAQ by odino ")

(with-input-from-file "../file-tests/abcdef" debugtest-query)
(check-equal? 
 (magic-result-output-text (with-input-from-file "../file-tests/abcdef" debugtest-query))
 "bc \bdef ")

(with-input-from-file "../file-tests/test.bin" debugtest-query)
(check-equal? 
 (magic-result-output-text (with-input-from-file "../file-tests/test.bin" debugtest-query))
 "test.bin determined with indirect relative offset test ")

(with-input-from-file "../file-tests/octal-test.txt" debugtest-query)
(check-equal? 
 (magic-result-output-text (with-input-from-file "../file-tests/octal-test.txt" debugtest-query))
 "octal test passed ")

(with-input-from-file "../file-tests/mnwx" debugtest-query)
(check-equal? 
 (magic-result-output-text (with-input-from-file "../file-tests/mnwx" debugtest-query))
 "string greater than passed ")


(with-input-from-file "tga2_extension" tgaext-query)
(check-equal? 
 (magic-result-output-text (with-input-from-file "tga2_extension" tgaext-query))
 "- author \"Jonathan Simpson\" - comment \"This is a targa version 2 test of the extension ar\" 6 \b-11 \b-2019 11 \b:07 \b:00 ")

(with-input-from-file "mm3.exe" name-query)
(check-equal? 
 (magic-result-output-text (with-input-from-file "mm3.exe" name-query))
 "MZ match, leshort match, nummask match ")

(with-input-from-file "FAQ-by--odino.txt" image-query)

(with-input-from-file "DOOM.EXE" exe-query)
(check-equal? 
 (magic-result-output-text (with-input-from-file "DOOM.EXE" exe-query))
 "MZ executable (MS-DOS) \b, LE for MS-DOS, DOS4GW DOS extender (embedded) ")

;; regex tests
;; ------------------------------------------------------------------
(with-input-from-file "ansi" regex-query)
(check-equal? 
 (magic-result-output-text (with-input-from-file "ansi" regex-query))
 "Compiled terminfo entry \"ansi\" ")

(with-input-from-file "wizardry.pnm" regex-query)
(check-equal? 
 (magic-result-output-text (with-input-from-file "wizardry.pnm" regex-query)) 
 "Netpbm image data \b, size = 320 x \b 200 \b, rawbits, pixmap ")

(with-input-from-file "winehid.inf" regex-query)
(check-equal? 
 (magic-result-output-text (with-input-from-file "winehid.inf" regex-query)) 
 "Windows setup INFormation ")

(with-input-from-file "Makefile" regex-query)
(check-equal? 
 (magic-result-output-text (with-input-from-file "Makefile" regex-query)) 
 "makefile script text ")

(with-input-from-file "wizwar.lnk" date-query)
(check-equal? 
 (magic-result-output-text (with-input-from-file "wizwar.lnk" date-query))
 "MS Windows shortcut \b, Item id list present \b, Points to a file or directory \b, Has Working directory \b, Has command line arguments \b, Icon \b number=0 \b, ctime=Monday, January 1st, 1601 12:00:00am \b, mtime=Monday, January 1st, 1601 12:00:00am \b, atime=Monday, January 1st, 1601 12:00:00am \b, length=0, window= \bhide ")

(with-input-from-file "CLIPBRD.HLP" date-query)
(check-equal? 
 (magic-result-output-text (with-input-from-file "CLIPBRD.HLP" date-query))
 "MS Windows 3.0 help \b, Monday, March 2nd, 1992 5:31:38am ")

(with-input-from-file "3dsfetch.smdh" string-query)
(check-equal? 
 (magic-result-output-text (with-input-from-file "3dsfetch.smdh" string-query))
 "Nintendo 3DS SMDH file \b: \"3dsfetch\" by VideahGams ")

(with-input-from-file "sym.sis" string-query)
(check-equal? 
 (magic-result-output-text (with-input-from-file "sym.sis" string-query))
 "Symbian .sis file ")

(with-input-from-file "enl_tr.jpg" jpeg-query)
(check-equal? 
 (magic-result-output-text (with-input-from-file "enl_tr.jpg" jpeg-query))
 "JPEG image data \b, JFIF standard \b 1. \b01 \b, aspect ratio \b, density 1x \b1 \b, segment length 16 \b, comment: \"CREATOR: XV Version 3.10-PCD-Magic2  Rev: 2/Dec/96  Quality = 95, Smoothing = 0\n\" \b, baseline, precision 8 \b, 1152x \b900 \b, frames 3 ")

(with-input-from-file "iexplore.exe" exe-query-all)
#;(check-equal? 
 (with-input-from-file "iexplore.exe" exe-query-all)
 "PE executable (MS-Windows) for AMD64 *** *** ")
