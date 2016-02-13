#lang racket

(require racket/cmdline)
(require slideshow/slides-to-picts)
(require racket/gui/base)
(require pict)

; set default parameters (can change on cmdline)
(define height (make-parameter 300))
(define width (make-parameter 400))
(define nap-time (make-parameter 1))

(define fn
  (command-line
   #:program "slide-watcher"
   #:once-each
   [("-x" "--width") x "Width of viewing window" (width (string->number x))]
   [("-y" "--height") y "Height of viewing window" (height (string->number y))]
   [("-s" "--sleep") seconds "Time to sleep between updates" (nap-time (string->number seconds))]
   #:ps "\nKeyboard commands:\n   q, Esc: quit\n   left, right: move one slide\n   1, g: first, last slide"
   #:args (slides)
   slides))

; global set of slides (as picts)
(define (get-slide-picts)
  (get-slides-as-picts fn (width) (height) #t))
(define slide-picts (get-slide-picts))
(define current-slide-n 0)

; update the global set of slides
(define (update-slide-picts) (set! slide-picts (get-slide-picts)))

; get the nth slide and turn it into a bitmap for plotting
(define (get-bitmap-slide n)
  (pict->bitmap (list-ref slide-picts n)))

; a channel to kill the update thread
(define exit-ch (make-channel))

; the window that the slide will show in
(define frame (new frame%
                   [label "slide"]
                   [height (height)]
                   [width (width)]))

; the canvas that goes inside that window
(define my-canvas
  (new (class canvas%
         ; to move through the slides
         (define (set-and-update n)
           (set! current-slide-n n)
           (send (send this get-parent) refresh))
         ; watch for key events when in focus
         (define/override (on-char key-event)
           (case (send key-event get-key-code)
             ['left (when (> current-slide-n 0)
                      (set-and-update (- current-slide-n 1)))]
             ['right (when (< current-slide-n (- (length slide-picts) 1))
                       (set-and-update (+ current-slide-n 1)))]
             [(#\g #\G) (set-and-update (- (length slide-picts) 1))]
             ['#\1 (set-and-update 1)]
             [(escape #\q #\Q) (begin
                                 (channel-put exit-ch (void))
                                 (send (send this get-parent) show #f))]))
         (super-new))
       [parent frame]
       [paint-callback
        ; when told to refresh, get the bitmap of the current slide
        (lambda (canvas dc)
          (send dc draw-bitmap (get-bitmap-slide current-slide-n) 0 0))]))

; display the window at startup
(send frame show #t)

; start up a thread that will watch if the file changes
(define update-thread
  (thread
   (local ((define fce (filesystem-change-evt fn)))
     (lambda ()
       (let loop ()
         ; loop, waiting either for a change in the file or an exit message
         (sync (handle-evt exit-ch (lambda (x) (void)))
               (handle-evt fce (lambda (x)
                                 (update-slide-picts)
                                 (send frame refresh)
                                 (sleep (nap-time))
                                 ; start a new file change watching event
                                 (set! fce (filesystem-change-evt fn))
                                 ; start over again
                                 (loop)))))))))