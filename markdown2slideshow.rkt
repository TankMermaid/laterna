#lang racket

(require parser-tools/lex
         ;(prefix-in sre: parser-tools/lex-sre)
         parser-tools/yacc)

(define-tokens a (LETTER FLAG))
(define-empty-tokens b (EOF BREAK))

(define lexer1
  (lexer
   [(union "% " "# " "## ") (token-FLAG lexeme)]
   [(char-complement "\n") (token-LETTER lexeme)]
   [(eof) (token-EOF)]
   [(repetition 1 +inf.0 "\n") (token-BREAK)]))

(define-struct line-exp (flag content))

(define parser1
  (parser
   (start lines)
   (end EOF)
   (tokens a b)
   (error (lambda args (map displayln args)))
   (grammar
    (lines ((line BREAK lines) (cons $1 $3))
           ((line BREAK) (list $1))
           ((line) (list $1)))
    (line ((FLAG content) (line-exp $1 $2))
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