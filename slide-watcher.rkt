#lang racket

(require racket/cmdline)
(require slideshow/slides-to-picts)
(require racket/gui/base)
(require pict)

(define height (make-parameter 300))
(define width (make-parameter 400))

(define fn
  (command-line
   #:program "slide-watcher"
   #:once-each
   [("-x" "--width") x "Width of viewing window" (width (string->number x))]
   [("-y" "--height") y "Height of viewing window" (height (string->number y))]
   #:args (slides)
   slides))

(define (get-slide-picts)
  (get-slides-as-picts fn width height #t))

(define slide-picts (get-slide-picts))
(define (update-slide-picts) (set! slide-picts (get-slide-picts)))
(define current-slide-n 0)

(define (get-bitmap-slide n)
  (pict->bitmap (list-ref slide-picts n)))

(define exit-ch (make-channel))

(define frame (new frame%
                   [label "slide"]
                   [height (height)]
                   [width (width)]))

(new (class canvas%
       (define/override (on-char key-event)
         (printf "on slide ~a of ~a~n" current-slide-n (length slide-picts))
         (case (send key-event get-key-code)
           ['left (when (> current-slide-n 0)
                    (set! current-slide-n (- current-slide-n 1))
                    (printf "switching to slide ~a~n" current-slide-n)
                    (send (send this get-parent) refresh))]
           ['right (when (< current-slide-n (- (length slide-picts) 1))                     
                     (set! current-slide-n (+ current-slide-n 1))
                     (printf "switching to slide ~a~n" current-slide-n)
                     (send (send this get-parent) refresh))]
           [(#\g #\G) (set! current-slide-n (- (length slide-picts) 1))
                      (send (send this get-parent) refresh)]
           ['#\1 (set! current-slide-n 1)
                 (send (send this get-parent) refresh)]
           [(escape #\q #\Q) (begin
                               (channel-put exit-ch (void))
                               (send (send this get-parent) show #f))]))
       (super-new))
     [parent frame]
     [paint-callback
      (lambda (canvas dc)
        (send dc draw-bitmap (get-bitmap-slide current-slide-n) 0 0))])
(send frame show #t)

(thread
 (local ((define fce (filesystem-change-evt fn)))
 (lambda ()
   (let loop ()
     (sync (handle-evt exit-ch (lambda (x) (void)))
           (handle-evt fce (lambda (x)
                             (update-slide-picts)
                             (send frame refresh)
                             (sleep 1)
                             (set! fce (filesystem-change-evt fn))
                             (loop))))))))