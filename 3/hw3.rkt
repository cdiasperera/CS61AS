#lang racket

(require simply-scheme)
(provide (all-defined-out))

; Exercise 1 - Define fast-expt-iter

(define (fast-expt-iter b n)
  (define (helper result base exp)
    (cond
      (
        (= exp 0)
        1
      )
      (
        (= exp 1)
        result
      )
      (
        (even? exp)
        (helper (* result (sqr base)) base (/ exp 2))
      )
      (
        else
        (helper (* result base) base (sub1 exp))
      )
    )
  )

  (helper 1 b n)

)

; Exericse 2 - Define phi
(define (phi)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
)

(define tolerance 0.00001)

(define (close-enough? v1 v2)
  (< (abs (- v1 v2)) tolerance))

(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; Exercise 3 - Define cont-frac

;; Recursive version
(define (cont-frac n d k)
  (define (helper count)
    (if (= count k)
      (/ (n k) (d k))
      (/ 
        (n count)
        (+
          (d count)
          (helper (add1 count))
        )
      )
    )
  )

  (helper 1)
)

;; Iterative version
(define (cont-frac-iter n d k)
  (define (iter val count)
    (if (zero? count)
      val
      (iter (/ (n count) (+ (d count) val)) (sub1 count))
    )
  )
  (iter (/ (n k) (d k) ) (sub1 k))
)


(define (inver-phi phi-gen k)
  (phi-gen
    (lambda (i) 1.0)
    (lambda (i) 1.0)
    k
  )
)

(define (find-phi-k phi-gen init) 
  (if (close-enough? (inver-phi phi-gen init) 0.618033988750)
    init
    (find-phi-k phi-gen (add1 init))
  )
)  

(define (e k)
  (define num (lambda (i) 1.0))
  (define denom (lambda (i)
    (if (zero? (modulo (- i 2) 3))
      (/ (+ (* 2 i) 2) 3)
      1
    )
  ))

  (+ 2 (cont-frac num denom k))
)

; Exercise 4 - Define next-perf



(define (next-perf n)
  (define (sum-of-factors i)
    (define (factor-of tester value)
      (zero? (modulo value tester))
    )

    (define (iter val n)
      (cond
        (
          (zero? n)
          val
        )
        (
          (factor-of n i)
          (iter (+ val n) (sub1 n))
        )
        (
          else
          (iter val (sub1 n))
        )
      )
    )

    ( iter 0 (sub1 i) )
  )


  (define (test-perf i)
    (= (sum-of-factors i) i)
  )

  (define (iter i)
    (if (test-perf i)
      i
      (iter (add1 i))
    )
  )

  (iter (add1 n))
)

; Exercise 5 - Explain what happens when the base cases are interchanged.

#|

In the original case, if the amount was 0, cc would return one, regardless of
how many different kinds of coins are present. In the latter, this guarding is
not present, so if the amount was 0 AND there no coins to test, you would return
1, which would be incorrect.

This would specifically happen if amount = 0 was given to the procedure. the
procedure would not call any situation where amount was 0, from inside itself.

Consider the else clause, where there are 2 calls to cc. The first call would
only give an amount of 0, if its calling procedure's stack had an amount of 0.
If this were the case, the [= amount 0] case would have caught it. Thus it
would not be called.

The second else call would have amount of 0, but it would not have an empty list
of coins (as it takes the list that is in the stack that called it).

|#

(define (count-change1 amount)
  (cc1 amount `(50 25 10 5 1)))

(define (count-change2 amount)
  (cc2 amount `(50 25 10 5 1)))

(define (cc1 amount kinds-of-coins)
  (cond [(= amount 0) 1]
        [(or (< amount 0) (empty? kinds-of-coins)) 0]
        [else (+ (cc1 amount
                     (bf kinds-of-coins))
                 (cc1 (- amount
                        (first kinds-of-coins))
                     kinds-of-coins))] ))

(define (cc2 amount kinds-of-coins)
  (cond
    [(or (< amount 0) (empty? kinds-of-coins)) 0]
    [(= amount 0) 1]
        [else (+ (cc2 amount
                     (bf kinds-of-coins))
                 (cc2 (- amount
                        (first kinds-of-coins))
                     kinds-of-coins))] ))

; Exercise 6 - Give a formula relating b, n, counter and product in expt-iter.

#|

Formula for expt: b^n = b^n * 1

Formula for expt-iter: b^n = product * b ^ counter

|#
