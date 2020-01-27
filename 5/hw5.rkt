#lang racket

(require simply-scheme)
(provide (all-defined-out))

(define square sqr)

; Accumulate with an init
(define (accum-init op init lst)
  (accumulate
    op 
    (append (list init) lst)
  )
)

;Exercise 1
;What are the result of the expressions? Make sure to comment your answer out.

; '(1 2 3 4 5 6)
; ( (1 2 3 ) 4 5 6 )
; ( (1 2 3) (4 5 6) )

; Exercise 2 Mobile

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; a. Define left-branch, right-branch, branch-length, and
; branch-structure.

(define (left-branch mobile)
  (car mobile)
)

(define (right-branch mobile)
  (cadr mobile)
)

(define (branch-length branch)
  (car branch)
)

(define (branch-structure branch)
  (cadr branch)
)

(define (weight? structure)
  (number? structure)
)

; The first value must be a number, for it to be a branch.
(define (branch? structure)
  (number? (car structure) )
)
; b. Define total-weight.

; Convert mobile into a list of weights in that mobile
(define (enumerate-mobile mobile)
  (cond
    (
      (weight? mobile)
      (list mobile)
    )
    (
      (branch? mobile)
      (enumerate-mobile (branch-structure mobile))
    )
    (
      else
      (let
        (
          (l-branch (left-branch mobile))
          (r-branch (right-branch mobile))
        )
        (append (enumerate-mobile l-branch) (enumerate-mobile r-branch))
      )
    )
  )
)

; For testing
(define left1 (make-branch 5 2))
(define right1 (make-branch 2 5))
(define mobile1 (make-mobile left1 right1))
(define left2 left1)
(define right2 (make-branch 7 mobile1))
(define mobile2 (make-mobile left2 right2))

(define (total-weight mobile)
  (accum-init
    +
    0
    (enumerate-mobile mobile)
  )
)
; c. Define balanced?
(define (torque branch)
  (let
    (
      (struct (branch-structure branch))
    )
    (let
      (
        (l (branch-length branch))
        (w (total-weight struct))
      )
      (* l w)
    )
  )
)

(define (balanced? mobile)
  (let
    (
      (l-branch (left-branch mobile))
      (r-branch (right-branch mobile))
    )
    (let
      (
        (l-struct (branch-structure l-branch))
        (r-struct (branch-structure r-branch))
      )
      (and
        (if (weight? l-struct)
          #t
          (balanced? l-struct)
        )
        (if (weight? r-struct)
          #t
          (balanced? r-struct)
        )
        (= (torque l-branch) (torque r-branch))
      )
    )
  )
)
; d. Redefine all the necessary procedures to work with the new
; constructors given below.
; Make sure that only one set of constructors is active at any time
; (otherwise Racket will complain about duplicate defintions).

;; (define (make-mobile left right)
;;   (cons left right))
;; (define (make-branch length structure)
;;   (cons length structure))

;; We would only have to change the selectors (and any procedures that check if
;; an element is a branch/mobile). And more specifically, in this case, we would
;; only NEED to change branch-structure and right-branch, as the others HAPPEN
;; to be correct for the new definitions.

;; (define (branch-structure branch) (cdr branch))
;; (define (right-branch mobile) (cdr branch))



;Exercise 3a - Define square-tree
(define (square-tree d-l)
  (map
    (lambda (el)
      (if (not (list? el))
        (sqr el)
        (square-tree el)
      )
    )
    d-l
  )
)
;Exercise 3b - Define tree-map

(define (tree-map fn tree)
  (map
    (lambda (el)
      (if (not (list? el))
        (fn el)
        (tree-map fn el)
      )
    )
    tree
  )
)
;Exercise 4 -  Complete the definition of accumulate-n
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    '()
    (cons 
      (foldr
        op init
        (foldl
          (lambda (lst res)
            (append res (list (car lst)))
          )
          '()
          seqs
        )
      )
      (accumulate-n op init
        (foldl
          (lambda (lst res)
            (append res (list (cdr lst)))
          )
          '()
          seqs
        )
      )
    )
  )
)


; For testing
(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
;Exercise 5 - Complete the definitions of matrix-*-vector, transpose,
; and matrix-*-matrix.

(define (dot-product v w)
  (foldr + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map 
    (lambda (vector) (dot-product v vector))
    m
  )
)

(define (transpose mat)
  (accumulate-n 
    se
    '()
    mat
  )
)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map
      (lambda (vector)
        (matrix-*-vector cols vector)
      )
      m
    )
  )
)


; For testing
(define v1 
  (list 1 2 3 4)
)

(define v2 
  (list 5 6 7 8)
)

(define v3
  (list 9 10 11 12)
)

(define m1
  (list v1 v2 v3)
)
;Exercise 6 - Give the property that op should satisfy:

#|

op should associative.

Associative: so the order the operands enter the operator doesn't matter

|#

;Exercise 7 - Define equal?

(define (my-equal? l1 l2)
  (cond
    ((null? l1) (null? l2))
    ((null? l2) (null? l1))
    (
      (and (list? l1) (list? l2))
      (and
        (my-equal? (car l1) (car l2))
        (my-equal? (cdr l1) (cdr l2))
      )
    )
    (
    else
    (eq? l1 l2)
    )
  )
)

;Exercise 8 - Complete the definition of subsets
(define (subsets s)
  (if (null? s)
    (list empty)
    (let 
      (
        (rest (subsets (cdr s ) ) )
      )
      (append
        rest
        (map
          (lambda (el)
            (se
              (car s)
              el
            )
          )
          rest
        )
      )
    )
  )
)

; For testing
(define set1 (list 1 2 3))

;Exercuse 9 - Modify the calc program
;; Racket calculator -- evaluate simple expressions

(define (calc)
  (flush-output)
  (print (calc-eval (read)))
  (newline)
  (calc))

; Evaluate an expression:

(define (calc-eval exp)
  (cond 
    (
      (number? exp) exp
    )
	  (
      (list? exp) (calc-apply (car exp) (map calc-eval (cdr exp)))
    )
    (
      (word? exp) exp
    )
	  (
      else (error "Calc: bad expression: " exp)
    )
  )
)

; Apply a function to arguments:

(define (calc-apply fn args)
  (cond 
    (
      (eq? fn '+) (foldr + 0 args)
    )
		(
      (eq? fn '-) 
      (cond
        ((null? args) (error "Calc: no args to -"))
			  ((= (length args) 1) (- (car args)))
			  (else (- (car args) (foldr + 0 (cdr args))))
      )
    )
		(
      (eq? fn '*)
      (foldr * 1 args)
    )
		(
      (eq? fn '/)
      (cond
        ((null? args) (error "Calc: no args to /"))
			  ((= (length args) 1) (/ (car args)))
        (else (/ (car args) (foldr * 1 (cdr args))))
      )
    )
    (
      (eq? fn 'first)
      (first (car args))
    )
    (
      (eq? fn 'butfirst)
      (butfirst (car args))
    )
    (
      (eq? fn 'last)
      (last (car args))

    )
    (
      (eq? fn 'butlast)
      (butlast (car args))

    )
    (
      (eq? fn 'word)
      (accum-init word "" args)
    )
		(else (error "Calc: bad operator:" fn)))
)