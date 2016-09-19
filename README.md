# Laterna

Magic for making slideshows with Racket

## Motivation and background

I'm a scientist, so I make a fair number of presentations. I'm also a purist
and a nerd, so it makes me crazy to design my presentations in PowerPoint,
which I find clunky and often infuriating. My slides are mostly a little bit of
text and a few images, nicely arranged. That information should be `grep`able
and easily transportable.

I therefore love Racket's [slideshow](http://docs.racket-lang.org/slideshow),
which I'll summarize as "the presentation is a program". Imagine Beamer but
more succint.

My problem with slideshow is that it lacks a lot of the common functionality I
use with slides. I want a title slide, transition slides, images that are
scaled relative to the page, etc. I slowly collected all this functionality in
Laterna.

## Getting started

### Installing
You can install Laterna like any other package:

1. Download it.
2. [Install it](https://docs.racket-lang.org/pkg/getting-started.html) via `raco` or, easier, via DrRacket.

### Using it

Replace your `(require slideshow)` and whatever else with just `(require
laterna)`. The things that Laterna provides that I think are particularly
useful are:

- *slide/title*. `(slide "Meter in Tennyson" items...)` is like `(slide #:title "Meter in Tennyson" items...)`. Laterna's title processing is also takes care of Markdown-style italics and bold: words in the title are by default is bold. Words inside starts become `*italics*`, double-star becomes `**normal**`.
- *transition-slide*. It takes just one string and puts that string in the middle of the slide.
- *bitmap/relative-width*. It takes a filename and a float. It scales the bitmap to that float's width relative to the slide's width.
- *with-ref*. It takes a pict and a string. The string gets put as a mini-caption to the bottom-right, just like you would do to give a citation or reference for a picture.
- *enum*. Takes a number and some text, so `(enum 1 "Iambic") (enum 2 "Spondaic")` feels like writing a list in markdown.
- *title-slide*. Takes a title, which is typeset differently, and then the rest of the content like normal.
- *pin-over\**. It's like `pin-over`, but it can take many specs.

## To-do

- Needs nicer documentation.
- More configurability.

## Author

[Scott Olesen](http://www.scottolesen.com)
