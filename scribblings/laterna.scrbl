#lang scribble/manual

@require[@for-label[laterna
                    racket/base]]

@title{Laterna}
@author{Scott W. Olesen}

@defmodule[laterna]

Maybe this should be @racket[slideshow/laterna]?

@defform[(slide/title title content ...)]{
It's quite simply:
@codeblock{(slide #:title title content ...)}
Most of my slides have titles so I just got tired of writing the keyword "title"
all the time.}

@defform[(bitmap/relative path scale)]{
 Loads the bitmap at the @racket[path] then rescales it so that the height (or width)
of the bitmap relative to the slide area (whichever is greater) ends up at the desired
@racket[scale].

I use this to put one big image in my slide. I want it to take up most of the space,
so I extend to almost the edges of the slide:
@codeblock{(slide/title "This slide has big picture"
             (bitmap/relative "img/big-picture.png" 0.9))}

If the bitmap is relatively tall, it will get squashed based on its height; if
relatively wide, by its width.}