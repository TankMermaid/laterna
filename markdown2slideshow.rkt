#lang racket

(require markdown)

(define inp "# Slide 1
Some pre-item words:

- Item 1
- Item 2

# Slide 2
Some words.

- An
- Ordered
- List

And other words.")

(define xs (parse-markdown inp))

(define (reparse x)
  (case (car x)
    ['h1 `(slide ,(third x))]
    ['p `(para ,(third x))]
    ['ul (map reparse (cddr x))]
    ['li `(item ,(third x))]))

; parse into slides
(define (slidify xs)
  (letrec ([f (lambda (xs slides slide-title slide-contents)
             (cond
               [(and (empty? xs) (null? slide-title)) slides]
               [(empty? xs) (cons `(slide #:title ,slide-title ,@slide-contents) slides)]
               [(and (null? slide-title) (equal? 'slide (caar xs))) (f (rest xs) '() (cadar xs) '())]
               [(equal? 'slide (caar xs)) (f (rest xs) (cons `(slide #:title ,slide-title ,@slide-contents) slides) (cadar xs) '())]
               [else (f (rest xs) slides slide-title (append slide-contents (list (car xs))))]))])
    (reverse (f xs '() null '()))))

(define ys (flatten (map reparse xs)))
(pretty-print (slidify (map reparse xs)))