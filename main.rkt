#lang slideshow

(provide slide/title
         transition-slide
         bitmap/scale
         bitmap/relative
         bitmap/relative-width
         with-ref
         enum
         title-slide
         pin-over*)

(require slideshow/text)
(require racket/draw)
(require "markdown.rkt")

(provide (all-from-out racket/draw)
         (all-from-out slideshow/text))


; a shorthand for defining the title, which most slides should have
(define-syntax-rule (slide/title title x ...)
  (slide #:title title x ...))

(define-syntax-rule (transition-slide title)
  (slide (big (italic (para #:align 'center title)))))

(define-syntax-rule (bitmap/relative-width filename relative-width)
  (let* ([bm (bitmap (read-bitmap filename))]
         [new-scale (/ (* relative-width (client-w)) (pict-width bm))])
    (scale bm new-scale)))

(define-syntax-rule (bitmap/relative filename relative-scale)
  (let* ([bm (bitmap (read-bitmap filename))]
         [relative-width (/ (pict-width bm) (client-w))]
         [relative-height (/ (pict-height bm) (pict-height (titleless-page)))]
         [relative-max (max relative-width relative-height)]
         [new-scale (/ relative-scale relative-max)])
    (scale bm new-scale)))

(define (with-ref pict ref #:margin [margin 0]) ; margin is for the pict
  (if (zero? margin)
      (vr-append
       pict
       (with-scale 0.5 (t ref)))
      (vr-append
       (vc-append
        pict
        (blank (pict-width pict) (* margin (pict-height pict))))
       (with-scale 0.5 (t ref)))))

(define-syntax-rule (bitmap/scale filename scale)
  (bitmap (read-bitmap filename #:backing-scale (/ 1.0 scale))))

; swo> bake this into bitmap/scale with an option
(define-syntax-rule (bitmap/scale/ref filename scale ref)
  (vr-append
   (bitmap/scale filename scale)
   (with-scale 0.5 (t ref))))

; title is aligned to the left of the whole slide, size 40
; uses markdown syntax for the title
(current-titlet
  (lambda (title)
    (para #:width (client-w) #:fill? #t (with-size 40 (parse-markdown-string title #:state 'bold)))))

(define-syntax-rule (enum i x0 ...)
  (let ([i2 (cond
              [(string? i) i]
              [(number? i) (number->string i)])])
    (item #:bullet (bt (string-append i2 ".")) x0 ...)))

(define (title-slide title . subtitles)
  (let ([x0 (with-size 40 (para #:align 'left (bold-italic-title title)))]
        [rest (map
               (lambda (st) (colorize (para #:align 'left (with-size 36 (bt st))) "gray"))
               subtitles)])
    (apply slide (cons x0 rest))))

(define (pin-over* #:base [base titleless-page] specs)
  (let* ([this-spec (first specs)]
         [rest-specs (rest specs)]
         [n-rest (length rest-specs)])
    (match this-spec
      [(list dx dy pict)
       (cond
         [(equal? n-rest 0) (pin-over base dx dy pict)]
         [(> n-rest 0) (pin-over (pin-over* #:base base rest-specs) dx dy pict)])])))

(current-main-font "Lato")
(set-page-numbers-visible! #f)


