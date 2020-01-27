#lang racket

(require simply-scheme)
(provide (all-defined-out))

;Exercise 0
;Write 5 expressions whose values are the number ten:
;1. Atom
10
;2. Compound Expression (3 Atoms)
(+ 1 9)
;3. Compound Expression (4 Atoms)
(+ 1 1 8)
;4. Compound Expression (1 Atom and 2 subexpressions)
(+ (+ 1 1) (+ 4 4))
;5. Any Other Kind Expression
(if #t 10 11)

;Exercise 1
(define (second wd)
  (first (bf wd)))
;1. Define first-two
(define (first-two wd)
  (word 
    (first wd) (first (bf wd))
  )
)

;;2. Define two-first
(define (two-first wd1 wd2)
  (word
    (first wd1) (first wd2)
  )
)

;;3. Define two-first-sent
(define (two-first-sent sent)
  (word
    (first (item 1 sent))
    (first (item 2 sent))
  )
)

;Exercise 2 - Define teen?
(define (teen? num)
  (and
    (>= num 13) (<= num 19)
  )
)

;Exercise 3 - Define indef-article
(define (vowel-first? wd) (member? (first wd) '(a e i o u) ) )
(define (indef-article wd)
  (se 
  (cond
    ((vowel-first? wd) 'an)
    (else 'a)
  )
  wd
  )
)

;Exercise 4 - Define insert-and
(define (insert-and sent)
  (se
    (bl sent)
    'and
    (last sent))
)

;Exercise 5 - Define query
(define (query sent)
  (se
    ; Swap the first two words with the below lin
    (first (bf sent))
    (first sent)
    ; Return all words but the first two words and the last word
    (bl(bf (bf sent)))
    ; Add a question mark to the last word
    (word
      (last sent)
      '?
    )
  )
)

;Exercise 6 - Define european-time and american-time
(define (european-time time)
  (define stand-time
    (cond
      ((equal? time '(12 am)) '(0 am))
      ((equal? time '(12 pm)) '(0 pm))
      (else time)
    )
  )
  (+
    (first stand-time)
    ; Add 12 hours to the time, if pm. Else, add 0 hours
    (if (equal? (last stand-time) 'pm)
      12
      0
    )
  )
)

(define (american-time time)
  (define beforenoon
    (if (< time 12)
      #t
      #f
    )
  )
  (define stand-time
    (cond
      ((equal? time 0) 12)
      ((equal? time 12) 24)
      (else time) 
    )
  )
  (if beforenoon
    (se
      stand-time
      'am
    )
    (se
      (- stand-time 12)
      'pm
    )
  )
)

;Exercise 7 - Define describe-time
(define (describe-time secs)
  (define second-max 60.0)
  (define minute-max (* second-max 60.0))
  (define hour-max (* minute-max 24.0))
  
  (cond
    ((< secs second-max) (se secs 'seconds))
    ((< secs minute-max) (se (/ secs second-max) 'minutes))
    ((< secs hour-max) (se (/ secs minute-max) 'hours))
    (else (se (/ secs hour-max) 'days))
  )
)

;Exercise 8 - Explain why superlative doesnt work:
(define (superlative adjective wd)
  (se (word adjective 'est) wd))

#|

In the original defintion, one of the arguments was 'word'. This has already
been defined as a procedure (in the simply-scheme library), thus, when 'word' is
invoked, the interpretor comes across word, it thinks it is the operand and not 
the procedure.

|#