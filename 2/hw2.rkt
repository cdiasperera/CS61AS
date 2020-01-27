#lang racket

(require simply-scheme)
(provide (all-defined-out))



; Exercise 1 - Define substitute
(define (substitute sent old-word new-word)
  
  (let (
      (stop-cond (lambda (sent) (equal? '() sent))) 
      (iterate-cond (lambda (first-wd wd) (equal? first-wd wd)) ))
    (cond
      ([stop-cond sent]
        '()
      )
      ( [iterate-cond (first sent) old-word]
        (se
          new-word (substitute (bf sent) old-word new-word)
        )
      )
      (else 
        (se
          (first sent) (substitute (bf sent) old-word new-word)
        )
      )
    )
  )
)


; Exercise 2 - Try out the expressions!

#|
(lambda (x) (+ x 3))
-> returns: A procedure

((lambda (x) (+ x 3)) 7)
-> returns: 10

(define (make-adder num)
  (lambda (x) (+ x num))) 
((make-adder 3) 7)
-> returns: 10

(define plus3 (make-adder 3)) 
(plus3 7)
-> returns: 10

(define (square x) (* x x)) 
(square 5)
-> returns: 25

(define square (lambda (x) (* x x))) 
(square 5)
-> returns: 25

(define (try f) (f 3 5)) 
(try +)
-> returns: 8

(try word)
-> returns: 35
|#


; Exercise 3
#|

Number of arguments g has: 0

Type of value returned by g: a procedure that has 1 argument, that would
take in 1 and return 3. For example: (define (g) (lamba (x) (+ x 2)))

|#

; Exercise 4 - Define f1, f2, f3, f4, and f5
; f1 can be any valid expression or procedure, to not cause error. 
; f1 will resolve to 1
(define f1 1)

; f2 must be a procedure that takes no arguments
; f2 will resolve to 2
(define f2 (lambda () 2))

; f3 must be a procedure that takes in 1 argument
; (f3 3) will resolve to 3
(define f3 identity)

; f4 must be a procedure that takes no arguemnts and returns a procedure that
; takes no arguments
; ((f4)) will resolve to 4
(define f4 (lambda () (lambda () 4)))

; f5 must be procedure that takes no arguements and returns a procedure that
; takes no arguemnts that returns a procedure that takes 1 argument
; (((f5)) 3) will resolve to 5
(define f5 
  (lambda () 
    (lambda ()
      (lambda (x)
        (+ x 2)
      )
    )
  )
)




; Exercise 5 - Try out the expressions
(define (t f) 
  (lambda (x) (f (f (f x)))) )

#|
1. ((t add1) 0) returns: 3

2. ((t (t add1)) 0) returns: 9

3. (((t t) add1) 0) returns: 27

|#

; Exercise 6 - Try out the expressions

(define (s x)
  (+ 1 x))

#|

1. ((t s) 0) returns: 3

2. ((t (t s)) 0) returns: 9

3. (((t t) s) 0) returns: 27

|#

; Exercise 7 - Define make-tester

(define (make-tester wd)
  (lambda (x)
    (equal? x wd)
  )
)

; Exercise 8 - SICP exercises

; SICP 1.31a
(define (product term a next b)
  (if (> a b)
    1
    (* (term a)
      (product term (next a) next b)
    )
  )
)

(define (estimate-pi)
  (define (term a)
    (let
      (
        (numer 
          (if (odd? a)
            (+ a 1)
            (+ a 2)
          )
        )
        (denom
          (if (even? a)
            (+ a 1)
            (+ a 2)
          )
        )
      )
      (/ numer denom)
    )
  )

  (let
    (
      (pi-by-4
        (product
          term
          1
          add1
          100
        )
      )
    )
    (* pi-by-4 4.0)
  )
)

; SICP 1.32a
(define (my-accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner
      (term a)
      (my-accumulate combiner null-value term (next a) next b)
    )
  )
)

;; Write sum in terms of my-accumulate:
(define (sum-accum term a next b)
  (my-accumulate + 0 term a next b)
)

;; Write product in terms of my-accumulate:
(define (product-accum term a next b)
  (my-accumulate * 1 term a next b)
)


; SICP 1.33
(define (filtered-accumulate combiner null-value term a next b pred)
  (if (> a b)
    null-value
    (combiner
      (if (pred a)
        (term a)
        null-value
      )
      (filtered-accumulate combiner null-value term (next a) next b pred)
    )
  )
)

(define (rel-prime? x y)
  (= (gcd x y) 1))

(define (prod-of-some-numbers n)
  (filtered-accumulate
    *
    1
    identity
    1
    add1
    n
    (lambda (x) (rel-prime? x n))
  )
)
; SICP 1.40 - Define cubic

(define (cubic a b c)
  (lambda (x)
    (+
      (expt x 3)
      (* a x x)
      (* b x)
      c
    )
  )
)

; SICP 1.41 - Define double

(define (double proc)
  (lambda (x) (proc (proc x)))
)

; SICP 1.43 - Define repeated
(define (compose proc2 proc1)
  (lambda (x)
    (proc2 (proc1 x))
  )
)

(define (my-repeated proc n)
  (if (equal? n 1)
    proc
    (compose proc (my-repeated proc (- n 1)))
  )
)

; Exercise 9 - Define my-every
(define (my-every proc sent)
  (let 
    (
      [empty-sent? (equal? sent '() )]
    )
    (if empty-sent?
      '()
      (se
        (proc (first sent))
        (my-every proc (bf sent))
      )
    )
  )
)

; Exercise 10 - Try out the expressions
#|

(every (lambda (letter) (word letter letter)) 'purple)
-> returns: 'pp uu rr pp ll ee

(every (lambda (number) (if (even? number) (word number number) number))
       '(781 5 76 909 24))
-> returns: '(781 5 7676 909 2424)

(keep even? '(781 5 76 909 24))
-> returns: '(76 24)

(keep (lambda (letter) (member? letter 'aeiou)) 'bookkeeper)
-> returns: 'ooeee

(keep (lambda (letter) (member? letter 'aeiou)) 'syzygy)
-> returns: ""

(keep (lambda (letter) (member? letter 'aeiou)) '(purple syzygy))
-> returns: Error, as it passes a word to member? as opposed to a letter

(keep (lambda (wd) (member? 'e wd)) '(purple syzygy))
-> returns: '(purple)
|#
