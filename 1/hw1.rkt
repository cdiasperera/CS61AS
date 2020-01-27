#lang racket

(require simply-scheme)
(provide (all-defined-out))

; Exercise 1 - Define dupls-removed
(define (dupls-removed sent)

  ; in? checks if a word is in the rest of the sentence
  (define (in? wd sent)
    (cond
      ((equal? '() sent) #f)
      ((equal? wd (first sent)) #t)
      (else (in? wd (bf sent)))
    )
  )

  ; incl-wd, or included word, is the word (or lack thereof) to attach to the
  ; list of non-duplicated words
  (define (incl-wd)
    (if (in? (first sent) (bf sent))
      '()
      (first sent)
    )
  )
  (if (equal? sent '())
    '()
    (se (incl-wd) (dupls-removed (bf sent)))
  )
)

; Exercise 2 - Define count-word
(define (count-word sent wd)
  ; The numerical value of this 'state.
  (define (state-val)
    (if (equal? wd (first sent))
      1
      0
    )
  )

  (if (equal? sent '())
    0
    (+ (state-val) (count-word (bf sent) wd))
  )
)

; Exercise 3

(define (pigl wd)
  (if (pl-done? wd)
      (word wd 'ay)
      (pigl (word (bf wd) (first wd)))))

(define (pl-done? wd)
  (vowel? (first wd)))

(define (vowel? letter)
  (member? letter '(a e i o u)))

; Explain what would happen if you used new-if instead of if below.
#|
  As new-if is a primitive produre, and not a special-form, it evaluates each
  argument as it comes across it. Thus, when it evaluates the 'else-case', it
  calls pigl recursively, ALWAYS, regardless if the predicate in the if is true
  or false. Thus, we get an ininite loop, which calls pigl while cycling the
  letters of the word.
|#

; Exercise 4 - Define squares

(define (squares sent)
  (if (equal? sent '())
    '()
    (se
      (* (first sent) (first sent))
      (squares (bf sent))
    )
  )
)

; Exercise 5 - Define switch

(define (switch sent)
  ; Returns an imperfect switched statement, that doesn't handle the start of
  ; the sentence beginning with I.
  (define (gen-switch sent)
    (cond
      ((equal? sent '())
        '()
      )
      ((or (equal? (first sent) 'I) (equal? (first sent) 'me)) 
        (se
          'you
          (gen-switch (bf sent))
        )
      )
      ((equal? (first sent) 'you)
        (se
          'me
          (gen-switch (bf sent))
        )
      )
      (else
        (se
          (first sent)
          (gen-switch (bf sent))
        )
      )
    )
  )

  ; The imperfect switched statement
  (define imperf-switched (gen-switch sent))

  ; If the first word is me, it should be switched to I.
  (if 
    (and 
      (not (equal? '() imperf-switched)) 
      (equal? (first imperf-switched) 'me)
    )
    (se
      'I
      (bf imperf-switched)
    )
    imperf-switched
  )
)

; Exercise 6 - Define ordered?

(define (ordered? list)
  (define (second list)
    (first (bf list))
  )
  
  (cond
    ((equal? (length list) 1) #t)
    ((> (first list) (second list)) #f)
    (else (ordered? (bf list)))
  )
)

; Exercise 7 - Define ends-e

(define (ends-e sent)

  (define (word-ends-e? wd) (equal? (last wd) 'e))


  (cond
    ((equal? sent '()) '())
    ((word-ends-e? (first sent))
      (se
        (first sent)
        (ends-e (bf sent))
      )
    )
    (else (ends-e (bf sent)) )
  )
)

; Exercise 8

#|
  Consider the following expression:
  (or (= x 0) (= y (/ 1 x)))
  If x is 0, we know that the evaluation will complete at (= x 0) in a 
  special-form and thus won't evaluate (=y (/ 1 x)), which will result in an
  error. In an ordinary form, the second argument will evaluate, even if x is 0,
  and thus, error out. A similar test can be devised for the and case:
  (and (not (= x 0)) (not (= y (/ 1 x))))
  (The above is equivalent to the first case, through De Morgan's Law)
  By similar logic of above, this will return false, if x is 0, if and is a 
  special form, but will error out if it is an ordinary form.
|#
