#lang racket

;; A line starting with a semicolon is a "comment".  You write
;; comments in order to explain in English what your code does, and
;; Racket knows to ignore comments since they aren't part of the
;; program.

;; This tells Racket that you want to use words and sentences (which
;; are disabled by default).
(require (planet dyoo/simply-scheme))

;; This tells Racket that it should "know" about all the functions you
;; define in this file.  (Don't worry about this for now.)
(provide (all-defined-out))

;; Exercise 0 - Introduce yourself

#|

This is a comment that spans multiple lines.

1) What is your name?
Channa
2) What is your major?
CS
3) Are you a returning student? (i.e. Did you take 61AS last semester?)
Nah
4) What made you to take 61AS?
Fun
5) Tell us interesting things about yourself.
Climber!
|#

;; Make a followup on the "Hello World!" post on Piazza introducing yourself.


;; Exercise 1 - Define sum-of-squares
(define (square x) (* x x))
(define (sum-of-squares x y) (+ (square x) (square y)))

;; Exercise 2a - Define can-drive
(define (can-drive age) (if (>= age 16) '(Good to go) '(Not yet)))

;; Exercise 2b - Define fizzbuzz
(define (divisible? big small) (= (remainder big small) 0))
( define 
  (fizzbuzz num)
  (cond 
    ((divisible? num 15) 'fizzbuzz)
    ((divisible? num 3) 'fizz)
    ((divisible? num 5) 'buzz)
    (else num) 
  )
)
;; Exercise 3 - Why did the Walrus cross the Serengeti?

#|
Your answer here


|#

;; Exercise 4 - new-if vs if

#|
Your answer here

|#
