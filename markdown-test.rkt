#lang slideshow

(require rackunit
         slideshow/text
         "markdown.rkt")

(check-equal? (parse-markdown-string "hi *there* bob")
              (list (t "hi") (it "there") (t "bob"))
              "easy string")

(check-equal? (parse-markdown-string #:state 'bold "hi *there* bob")
              (list (bt "hi") (bit "there") (bt "bob"))
              "reversed bold")