#lang racket

(require (planet dyoo/simply-scheme))
(provide (all-defined-out))

;;Begin Project 1
(require "adjectives.rkt") 

;;Below is some procedures to help you out. Dont worry about what it says or does.
;;Questions are below.

(define (want-exit? line)
  (or (member? 'exit line) (member? 'quit line) (member? 'bye line)))

(define (print-sentence sent)
  (for-each (lambda (x) (display x) (display " "))
            sent)
  (newline))

(define (interact bot)
  (define (helper)
    (display "> ") (flush-output)
    (let ([line (read-line)])
      (unless (want-exit? line)
        (print-sentence (bot line))
        (helper))))
  (read-line)
  (helper))

(define (chatter bot1 bot2 start iterations)
  (define (helper b1 b2 sent i)
    (when (< i iterations)
          (display "bot ") (display (add1 (remainder i 2))) (display ": ")
          (let ((out (b1 sent)))
            (print-sentence out)
            (helper b2 b1 out (add1 i)))))
  (display "start: ") (print-sentence start)
  (helper bot1 bot2 start 0))

(define (create-iterator proc combiner next n)
  (lambda (x)
    (if (= n 1)
      (proc x)
      (combiner
        (proc x)
        ((create-iterator proc combiner next (- n 1)) (next x))
      )
    )
  )
)

(define (my-compose proc2 proc1)
  (lambda (x)
    (proc2 (proc1 x))
  )
)

(define (repeat proc n)
  (if (equal? n 1)
    proc
    (my-compose proc (repeat proc (- n 1)))
  )
)
; bfn = but first n items
; Returns a procedure, that takes a sentence, and returns the sentence but the
; first n items
(define (bfn n)
  (repeat bf n)
)

(define (firstn n)
  (create-iterator first se bf n)
)

;;; Checks if a given word is an adjective or not
;;; Requires adjectives.scm to be loaded beforehand
(define adjective?
  (let ((hash (make-hash)))
    (for-each (lambda (adj)
		(hash-set! hash adj #t))
	      adjectives)
    (lambda (wd) (hash-ref hash wd #f))
  )
)

(define (prepend-very-to-wd wd)
  (if (adjective? wd)
    (se
      'very
      wd
    )
    wd
  )
)

(define (apply-very-to-sent sent)
  (every prepend-very-to-wd sent)
)

;; Begin Questions:
;;Q1 - babybot
(define (babybot sent)
  sent
)

;;Q2 - stupidbot-creator
(define (stupidbot-creator motto)
  (lambda (sent)
    motto
  )
)

;;Q3 - matcherbot-creator
(define (matcherbot-creator pattern)
  (lambda (sent)
    (cond
      (
        (equal? '() pattern)
        sent
      )
      (
        (< (length sent) (length pattern))
        #f
      )
      (
        (equal? ( (firstn (length pattern)) sent) pattern)
        ((bfn (length pattern)) sent)
      )
      (else ((matcherbot-creator pattern) (bf sent)))
    )
  )
)


(define (check-n-switch-creator from to)
  (lambda (wd)
    (cond
      ((equal? '() from) wd)
      ((equal? (first from) wd) (first to))
      (else ((check-n-switch-creator (bf from) (bf to)) wd))
    )
  )
)

;;Q4 - substitutebot-creator
(define (substitutebot-creator from to)
  (lambda (sent)
    (every (check-n-switch-creator from to) sent)
  )
)

;;Q5 - switcherbot
(define (switcherbot sent)
  (define first-p '(me I am was my yours))
  (define second-p '(you you are were your mine))
  
  (define first-to-second ((substitutebot-creator first-p second-p) sent) )
  (define second-to-first ((substitutebot-creator second-p first-p) sent) )

  ; Select the correct word in the output, out of the the three options
  (define (select-nth-wd n)
    (define i (+ n 1))
    (let
      (
        (nth-first-to-second (item i first-to-second))
        (nth-second-to-first (item i second-to-first))
        (nth-sent (item i sent))
      )
      (cond
        ((not (equal? nth-first-to-second nth-sent)) nth-first-to-second)
        ((not (equal? nth-second-to-first nth-sent)) nth-second-to-first)
        (else nth-sent)
      )
    )
  )

  ; For every word position in the sentence, select the correct one to make the
  ; switched sentece
  (define almost-correct
    (let
      (
        (range-sent (range (length sent)))
      )
      (every select-nth-wd range-sent)
    )
  )
  
  ; Account for start-of-sentence grammer (I instead of me)
  (if (equal? '() sent)
    '()
    (if (equal? (first almost-correct) 'me)
      (se
        'I
        (bf almost-correct)
      )
      almost-correct
    )
  )
)

;;Q6 - inquisitivebot
(define (inquisitivebot sent)
  (se
    (switcherbot sent)
    (if (equal? '() sent)
     '()
      '?
    )
  )
)

;;Q7 - eliza
(define (eliza sent)
  (define reply-hello (stupidbot-creator '(hello there!)))

  (define get-after-I-am (matcherbot-creator '(I am)))
  (define sent-after-I-am (get-after-I-am sent))
  (cond
    (
      (equal? '() sent)
      '(how can I help you ?)
    )
    (
      (equal? (first sent) 'hello)
      (reply-hello sent)
    )
    (
      sent-after-I-am
      (se
        '(why are you)
        (inquisitivebot sent-after-I-am)
      )
    )
    (
      (equal? (last sent) '?)
      '(I can not answer your question.)
    )
    (
      else
      (switcherbot sent)
    )
  )
)

;;Q8 - reactorbot-creator
(define (reactorbot-creator bot pat out)
  (lambda (sent)
    (if (equal? sent pat)
      out
      (bot sent)
    )
  )
)

;;Q9 - replacerbot-creator
(define (replacerbot-creator bot pat before after)
  (define remove-before-incl-pat (matcherbot-creator pat))
  (lambda (sent)
    (define sent-after-pattern (remove-before-incl-pat sent))
    (define test 'a)
    (if sent-after-pattern
      (se
        before
        sent-after-pattern
        after
      )
      (bot sent)
    )
  )
)

;;Q10 - exagerate
(define (exaggerate bot n)
  (lambda (sent)
    (if (= n 0)
      (bot sent)
      (apply-very-to-sent ((exaggerate bot (- n 1)) sent))
    )
  )
)

;;REMEMBER TO ADD YOUR OWN TESTS TO GRADER.RKT!
;;END OF PROJECT 1