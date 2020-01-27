(define (add-rational r1 r2)
  (let
    (
      (numer1 (numer r1))
      (denom1 (denom r1))
      (numer2 (numer r2))
      (denom2 (denom r2))

    )
    (make-rational
      (+ (* numer1 denom2) (* numer2 denom1))
      (* denom1 denom2)
    )
  )
)

(define (add-complex c1 c2)
  (let
    (
      (r1 (real c1))
      (i1 (imag c1))
      (r2 (real c2))
      (i2 (imag c2))
    )
    (make-complex
      (+ r1 r2)
      (+ i1 i2)
    )
  )
)

(define (add-numbers n1 n2)
  (let
    (
      (type1 (type-tag n1))
      (type2 (type-tag n2))
    )
    (if (equal? type1 'rational)
      (if (equal? type2 'rational)
        (add-rational n1 n2)
        (add-rational-complex n1 n2)
      )
      (if (equal? type2 'rational)
        (add-rational-complex n1 n2)
        (add-complex n1 n2)
      )
    )
  )
)

; Evaluation of example 1 in 'How does Racket-1 work?'
;
#|

  ((lambda (x) (* x x)) (- 2 (* 4 3)))
  1) The subexpression (- 2 (* 4 3)) must be evaluated
  2) The subexpression (* 4 3) must be evaluated
  3) The * operator is applied to 4 and 3, and returns 12
  4) The - operator is applied to 2 and 12 and returns -10.
  5) We apply -10 to the lambda expression, resulting in the need to evaluate 
     (* -10 -10)
  6) The * operator is applied to -10 and -10, and returns 100.
|#

; Practice with Racket

#|

  ((lambda (x) (+ x 3)) 5)
  1) The above expression (hereby referred to as exp), as a quote, is sent to 
     the eval-1 procedure
  2) As the above has two elements (the lambda exp and 5), it is evaluated as a
     pair: (apply-1 (eval-1 (car exp)) (map eval-1 (cdr exp)) )
  3) The outermost eval-1 is evaluated first: (eval-1 (car exp)). This is the
     lambda function, and is returned as a quote of such. 
  4) The second operand to apply-1 is evaluated next: (map eval-1 (cdr exp)).|
  5) As cdr exp is simply 5 and is treated as a constant, it is directly
     returned.|
  6) This leaves (apply-1 (lamda (x) (+ x 3)) 5)
  7) As the first operand is lambda experssion, we run:
     (eval-1 (substitute (caddr proc) (cadr proc) args '()))
  8) This evaluates to:
     (eval-1 (substitute (+ x 3) (x) 5))
  9) This evaluates to (eval-1 (+ 5 3)), which evaluates to 8.

|#