#lang racket

(require simply-scheme)
(provide (all-defined-out))

; Exercise 1

; SICP 2.7 - Define upper-bound and lower-bound

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (make-interval a b) (cons a b))

(define (upper-bound interval)
  (cdr interval)
)

(define (lower-bound interval)
  (car interval)
)
; SICP 2.8 - Define sub-interval

(define (sub-interval x y)
  (make-interval 
    (- (lower-bound x) (upper-bound y))
    (- (upper-bound x) (lower-bound y))
  )

)

; SICP 2.10 - Modify div-interval
(define (span-zero? interval)
  (or
    (and (<= (lower-bound interval) 0) (>= (upper-bound interval) 0))
    (and (<= (upper-bound interval) 0) (>= (lower-bound interval) 0))
  )
)

(define (div-interval x y)
  (if (span-zero? y)
    (error "You cannot divide an interval (first arg) with one that spans zero (second arg)")
    (mul-interval x 
                  (make-interval (/ 1 (upper-bound y))
                                (/ 1 (lower-bound y)))))
)


;SICP 2.12 - Define make-center-percent and percent

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (width-from-tol c tol)
  (* c (/ tol 100))
)

(define (make-center-percent c tol)
  (make-center-width c (width-from-tol c tol))
)

(define (percent interval)
  (* 100 (/ (/ (width interval) 2) (center interval)))
  (let
    (
      (c (center interval))
      (w (width interval))
    )
    (/ (* 100 w ) c)
  )
)
; SICP 2.17 - Define last-pair

(define (last-pair lst)
  (if (null? (cdr lst))
    (list (car lst))
    (last-pair (cdr lst))
  )
)
; SICP 2.20 - Define same-parity

(define (same-parity . nums)
  (define (parity-pred base)
    (if (even? base)
      even?
      odd?
    )
  )
  (filter (parity-pred (car nums)) nums)
)
; SICP 2.22 - Write your explanation in the comment block:

#|
For the first attempt, this is because Louis is processing the list from front
to back, but is attaching the 'previous' answer behind the one that is just
processed. Thus, at the end, the first result will be the last processed item in
the list, the second result will be the second last item processed, and so on,
resulting in a reversed list.

In the second attempt, the answer order is almost perfect. The issue is that
because the last item (which was nil) is at the front, the list appears to end
at the first element, when it should be the last element. The empty list is
treated as the first element, which could cause errors.
|#

; Exercise 2 - Define my-substitute



(define (replace replace-func old new)
  (lambda (item)
    (if (list? item)
      (map (replace replace-func old new) item)
      (replace-func item old new)
    )
  )
)

(define (substitute lst old new)
  (define (replace-wd wd old-wd new-wd)
    (if (equal? wd old-wd)
      new-wd
      wd
    )
  )
  (map (replace replace-wd old new) lst)
)
; Exercise 3 - Define my-substitute2

(define (substitute2 lst old new)
  (define (replace-sent wd old new)
    (cond
      ((empty? old) wd)
      ((equal? (first old) wd) (first new))
      (else (replace-sent wd (bf old) (bf new)))
    )
  )

  (map (replace replace-sent old new) lst)
)

(define (compose proc1 proc2)
  (lambda (x)
    (proc2 (proc1 x))
  )
)

(define (word-to-list word)
  (if (equal? "" word)
    '()
    (cons 
      (first word)
      (word-to-list (bf word))
    )
  )
)

(define (cxr-function cxr)
  (define (get-function letter)
    (cond
      (
        (or (equal? letter 'c) (equal? letter 'r))
        identity
      )
      (
        (equal? letter 'a)
        car
      )
      (
        (equal? letter 'd)
        cdr
      )
    )
  )

  (let
    (
      (cxr-functions (map get-function (word-to-list cxr)))
    )
    (foldr compose identity cxr-functions)
  )
)

