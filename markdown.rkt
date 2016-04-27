#lang slideshow

(require racket/draw
         slideshow/text)

(define (state? x)
  (member x '(normal bold italic bolditalic tt boldtt)))

(provide (contract-out
          [parse-markdown-string (->* (string?)
                                      (#:state state?)
                                      (listof pict?))]))

(define-struct mdchar (state char)
  #:methods gen:custom-write
  [(define (write-proc c port mode)
     (fprintf port "<mdchar ~a ~c>" (mdchar-state c) (mdchar-char c)))])

(define-struct mdrun (state str)
  #:methods gen:custom-write
  [(define (write-proc r port mode)
     (fprintf port "<mdrun ~a ~s>" (mdrun-state r) (mdrun-str r)))])

(define (parse-markdown-string str #:state [state 'normal])
  (let* ([chars (string->list str)]
         [mdchars (lex-chars state chars)]
         [words (split-mdchars mdchars)]
         [runs (map parse-mdruns words)]
         [picts (map mdruns->pict runs)])
    picts))

; take a list of runs and turn that into a pict
; if it's just one run, easy; otherwise, you need to append them together
(define (mdruns->pict runs)
  (if (zero? (length runs))
      (blank)
      (apply hb-append (map (λ (run) ((hash-ref state-function-hash (mdrun-state run)) (mdrun-str run))) runs))))

(define state-function-hash
  (make-hash (list (cons 'normal t)
                   (cons 'italic it)
                   (cons 'bold bt)
                   (cons 'bolditalic bit)
                   (cons 'tt tt)
                   (cons 'boldtt tt))))

; turn a list of mdchars into a set of runs
(define (parse-mdruns mdchars)
  (let ([chunks (chunk mdchars mdchar-state)])
    (map (match-lambda
           [(list mdchars state) (mdrun state (list->string (map mdchar-char mdchars)))]) chunks)))

; break up a list into "chunks" of equal values under pred
; returns (list (list first-elts value) ...)
(define (chunk lst pred)
  (define (iter storage val lst)
    (if (empty? lst)
        (list (list (reverse storage) val))
        (let ([first-val (pred (first lst))])
          (if (equal? val first-val)
              (iter (cons (first lst) storage) val (rest lst))
              (cons (list (reverse storage) val) (iter (list (first lst)) first-val (rest lst)))))))
  (if (empty? lst)
      '()
      (iter (list (first lst)) (pred (first lst)) (rest lst))))

(define (split-mdchars mdchars)
  (define (iter storage cs)
    (if (empty? cs)
        (list (reverse storage))
        (let ([first-is-space (equal? #\space (mdchar-char (first cs)))])
          (cond
            [(and first-is-space (empty? storage)) (iter '() (rest cs))]
            [first-is-space (cons (reverse storage) (iter '() (rest cs)))]
            [else (iter (cons (first cs) storage) (rest cs))]))))
  (iter '() mdchars))

(define (lex-chars state chars)
  (match chars
    ['() '()]
    [(list-rest #\* #\* #\* tail) (match state
                                    ['bolditalic (lex-chars 'normal tail)]
                                    ['normal (lex-chars 'bolditalic tail)])]
    [(list-rest #\* #\* tail) (match state
                                ['normal (lex-chars 'bold tail)]
                                ['bold (lex-chars 'normal tail)]
                                ['italic (lex-chars 'bolditalic tail)]
                                ['bolditalic (lex-chars 'italic tail)])]
    [(list-rest #\* tail) (match state
                            ['normal (lex-chars 'italic tail)]
                            ['italic (lex-chars 'normal tail)]
                            ['bold (lex-chars 'bolditalic tail)]
                            ['bolditalic (lex-chars 'bold tail)])]
    [(list-rest #\\ #\* tail) (cons (mdchar state #\*) (lex-chars state tail))]
    [(list-rest #\` tail) (match state
                            ['normal (lex-chars 'tt tail)]
                            ['tt (lex-chars 'normal tail)]
                            ['bold (lex-chars 'boldtt tail)]
                            ['boldtt (lex-chars 'bold tail)])]
    [(list-rest #\\ #\` tail) (cons (mdchar state #\`) (lex-chars state tail))]
    [(list-rest c tail) (cons (mdchar state c) (lex-chars state tail))]))