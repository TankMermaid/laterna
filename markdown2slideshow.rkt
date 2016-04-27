#lang racket

(require parser-tools/lex
         ;(prefix-in sre: parser-tools/lex-sre)
         parser-tools/yacc)

(define-tokens a (LETTER TITLE))
(define-empty-tokens b (EOF BREAK))

(define lexer1
  (lexer
   [(union "# " "## ") (token-TITLE lexeme)]
   [(char-complement "\n") (token-LETTER lexeme)]
   [(eof) (token-EOF)]
   [(repetition 1 +inf.0 "\n") (token-BREAK)]))

(define-struct line-exp (flag content))
(define-struct meta-exp (content))
(define-struct slide-exp (title content))
(define-struct title-exp (flag title))

(define parser1
  (parser
   (start lines)
   (end EOF)
   (tokens a b)
   (error (lambda args (map displayln args)))
   (grammar
    (slides ;((% line slides) (cons (meta-exp $2) $4))
            ((slide slides) (cons $1 $2))
            ((slide) (list $1)))
    (slide ((title) `(slide/title $1 '()))
           ((title content) (slide-exp $1 $2)))
    (title ((TITLE line) (title-exp $1 $2)))
    (lines ((line BREAK lines) (cons $1 $3))
           ((line BREAK) (list $1))
           ((line) (list $1)))
    (line ;((FLAG content) (line-exp $1 $2))
          ((content) (line-exp 'content $1)))
    (content ((LETTER content) (cons $1 $2))
             ((LETTER) (list $1)))
    )))

(define inp1 "# slide 1")

(define inp "# Slide 1
Some pre-item words:

- Item 1
- Item 2

# Slide 2
Some words.

- An
- Ordered
- List
")

(define lines
  (let* ([input (open-input-string inp)])
    (parser1 (lambda () (lexer1 input)))))

(for ([line lines])
  (let ([flag (line-exp-flag line)]
        [content (string-join (line-exp-content line) "")])
    (displayln content)))