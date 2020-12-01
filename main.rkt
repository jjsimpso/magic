#lang racket/base

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


(module reader racket/base
  (provide read-syntax)
  (require magic/reader)

  ;; stub out for DrRacket features (see jsonic example in Beautiful Racket)
  (define (get-info port src-mod src-line src-col src-pos)
    (define (handle-query key default)
      (case key
        #;[(color-lexer)
           (dynamic-require 'magic/colorer 'color-magic)]
        #;[(drracket:indentation)
           (dynamic-require 'magic/indenter 'indent-magic)]
        #;[(drracket:toolbar-buttons)
           (dynamic-require 'magic/buttons 'button-list)]
        [else default]))
    handle-query))
