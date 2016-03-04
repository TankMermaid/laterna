#lang racket/base

(require drracket/tool
         racket/class
         racket/gui/base
         racket/unit)
(provide tool@)

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
    (define showtop-mixin
      (mixin ((class->interface text%)) ()
        
        (inherit begin-edit-sequence
                 end-edit-sequence
                 insert
                 get-text)

        (define/augment (on-insert start len)
          (begin-edit-sequence))
        (define/augment (after-insert start len)
          (println "edited")
          (end-edit-sequence))

        (super-new)))

    (define (phase1) (void))
    (define (phase2) (void))

    (drracket:get/extend:extend-definitions-text showtop-mixin)))