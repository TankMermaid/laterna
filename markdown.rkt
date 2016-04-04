#lang slideshow

(require racket/draw
         slideshow/text)

(define (state? x)
  (member x '(normal bold italic bolditalic tt)))

(provide (contract-out
          [parse-markdown-string (->* (string?)
                                      (#:state state?)
                                     (listof pict?))]))

(define-struct run (state words))

(define (parse-markdown-string str #:state [state 'normal])
  (for ([run (lex-markdown-string str state)])
    (printf "lm | state: ~a words ~a\n" (run-state run) (run-words run)))
  (parse-runs (lex-markdown-string str state)))

(define (lex-markdown-string str state)
  (define (parse-chars state storage chars)
    (match chars
      ['() (if (empty? storage) '() (list (done-run state storage)))]
      [(list-rest #\* #\* #\* tail) (match state
                                      ['bolditalic (new-run 'bolditalic 'normal storage tail)]
                                      ['normal (new-run 'normal 'bolditalic storage tail)])]
      [(list-rest #\* #\* tail) (match state
                                  ['italic (new-run 'italic 'bolditalic storage tail)]
                                  ['bold (new-run 'bold 'normal storage tail)]
                                  ['bolditalic (new-run 'bolditalic 'italic storage tail)]
                                  ['normal (new-run 'normal 'bold storage tail)])]
      [(list-rest #\* tail) (match state
                              ['italic (new-run 'italic 'normal storage tail)]
                              ['bold (new-run 'bold 'bolditalic storage tail)]
                              ['bolditalic (new-run 'bolditalic 'bold storage tail)]
                              ['normal (new-run 'normal 'italic storage tail)])]
      [(list-rest #\\ #\* tail) (parse-chars state (cons #\* storage) tail)]
      [(list-rest #\` tail) (match state
                              ['italic (new-run 'italic 'italictt storage tail)]
                              ['italictt (new-run 'tt 'italic storage tail)]
                              ['bold (new-run 'bold 'boldtt storage tail)]
                              ;['boldtt (new-run 'tt 'bold storage tail)]
                              ['boldtt (new-run 'boldtt 'bold storage tail)]
                              ['bolditalic (new-run 'bolditalic 'bolditalictt storage tail)]
                              ['bolditalictt (new-run 'tt 'bolditalic storage tail)]
                              ['tt (new-run 'tt 'normal storage tail)]
                              ['normal (new-run 'normal 'tt storage tail)])]
      [(list-rest #\\ #\` tail) (parse-chars state (cons #\` storage) tail)]
      [(list-rest c tail) (parse-chars state (cons c storage) tail)]))
  (define (done-run state chars) (run state (string-split (list->string (reverse chars)) #:trim? #f)))
  (define (new-run old-state new-state old-chars new-chars)
    (cons (done-run old-state old-chars) (parse-chars new-state '() new-chars)))
  (parse-chars state '() (string->list str)))

(define state-function-hash
  (make-hash (list (cons 'normal t)
                   (cons 'italic it)
                   (cons 'bold bt)
                   (cons 'bolditalic bit)
                   (cons 'tt tt)
                   (cons 'italictt tt)
                   (cons 'boldtt tt)
                   (cons 'bolditalictt tt))))

(define (parse-words state words)
  (printf "pw | state: ~a words: ~a\n" state words)
  (map (hash-ref state-function-hash state) (filter non-empty-string? words)))

(define (hybrid-word state1 state2 word1 word2)
  (hb-append ((hash-ref state-function-hash state1) word1)
             ((hash-ref state-function-hash state2) word2)))
  
(define (parse-runs runs)
  (match runs
    ['() '()]
    [(list-rest (run _ '()) more-runs) (parse-runs more-runs)]
    [(list (run state words)) (parse-words state words)]
    [(list-rest (run state1 words1) (run state2 words2) more-runs)
     (if (not (or (equal? (length words1) 1)
                  (and (non-empty-string? (last words1))
                       (non-empty-string? (first words2)))))
         (append (parse-words state1 words1) (parse-runs (rest runs)))
         (let* ([new-run2 (run state2 (rest words2))]
                [new-rest-runs (cons new-run2 more-runs)])
           (printf "pr | mashing ~a and ~a\n" (last words1) (first words2))
           (append (parse-words state1 (drop-right words1 1))
                   (list (hybrid-word state1 state2 (last words1) (first words2)))
                   (parse-runs new-rest-runs))))]))