#lang info

(define collection "laterna")
(define deps '("base"
               "slideshow"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/laterna.scrbl" ())))
(define pkg-desc "Syntactic et al. tools for slideshow")
(define version "0.1")
(define pkg-authors '("Scott W. Olesen"))

(define drracket-tools (list (list "tool.rkt")))
