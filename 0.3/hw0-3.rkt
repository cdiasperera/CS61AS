#lang racket

(require simply-scheme)
(provide (all-defined-out))

; Constants the define the numbers of seconds in a min, hour, day
(define second-max 60.0)
(define minute-max (* second-max 60.0))
(define hour-max (* minute-max 24.0))

; Constants for unit display
(define sec 'SECONDS)
(define min 'MINUTES)
(define hr 'HOURS)
(define dy `DAYS)
; Exercise 1 - Define describe-time

; Function to get the approximate (to the nearest integer) description of time
(define (describe-time-remainder secs)
  (cond
    ((< secs second-max) (se secs sec))
    ((< secs minute-max) (se (quotient secs second-max) min))
    ((< secs hour-max) (se (quotient secs minute-max) hr))
    (else (se (quotient secs hour-max) dy))
  )
)

; Accurate (to the second) description of time
(define (describe-time secs)
  ; Define approximate time values
  (define described-time (describe-time-remainder secs))
  (define value (item 1 described-time))
  (define units (item 2 described-time))

  ; Find how many seconds are left after the approximation
  ; i.e: if the true time is 1.6 hours, how many seconds are in .6 hours?
  (define reduced-secs
    (cond
      ((equal? units dy) (- secs (* value hour-max)))
      ((equal? units hr) (- secs (* value minute-max)))
      ((equal? units min) (- secs (* value second-max)))
      ((equal? units sec) 0)
    )
  )

  ; If secs = 0, nothing left to display, in terms of time
  ; Otherwise, we describe an approximation and continue refining for what's
  ; left
  (if (= secs 0)
    '()
    (se
      (describe-time-remainder secs)
      (describe-time reduced-secs)
    )
  )

)

; Exercise 2 - Define remove-once
(define (remove-once wd sent)
  (if (equal? '() sent)
    '()
    (if (equal? wd (first sent))
      (bf sent)
      (se
        (first sent)
        (remove-once wd (bf sent))
      )
    )
  )
)

; Exercise 3 - Define differences
(define (differences nums)
  (if (or (equal? nums '()) (equal? (bf nums) '()))
    '()
    (se
      (- (item 2 nums) (item 1 nums))
      (differences (bf nums))
    )
  )
)

; Exercise 4 - Define location
(define (location wd sent)
  (cond
    ((equal? '() sent) #f)
    ((equal? wd (first sent)) 1)
    (else
      (and
        (location wd (bf sent))
        (+ 1 (location wd (bf sent))) 
      )
    )
  )
)

; Exercise 5 - Define initials
(define (initials sent)
  (if (equal? sent '())
    '()
    (se
      (first (first sent))
      (initials (bf sent))
    )
  )
)

; Exercise 6 - Define copies
(define (copies num wd)
  (if (equal? num 0)
    '()
    (se
      wd
      (copies (- num 1) wd)
    )
  )
)

; Exercise 7 - Define gpa
(define (gpa grades)
  (define (base-value grade)
    (cond
      ((equal? (first grade) 'A) 4)
      ((equal? (first grade) 'B) 3)
      ((equal? (first grade) 'C) 2)
      ((equal? (first grade) 'D) 1)
      ((equal? (first grade) 'F) 0)
    )
  )

  (define (value-mod grade)
    (cond
      ((equal? (bf grade) '+) 0.33)
      ((equal? (bf grade) '-) -0.33)
      (else 0)
    )
  )

  (define (grade-value grade)
    (+ (base-value grade) (value-mod grade))
  )

  (define (sum-grades grades)
    (if (equal? grades '())
      0
      (+ (first grades) (sum-grades (bf grades)))
    )
  )

  (define (get-grades-by-value grades)
    (if (equal? grades '())
      '()
      (se
        (grade-value (first grades))
        (get-grades-by-value (bf grades))
      )
    )
  )

  (define grades-by-value (get-grades-by-value grades))

  (/ (sum-grades grades-by-value) (length grades-by-value))
)

; Exercise 8 - Define repeat-words
(define (repeat-words sent)
  (define (repeat-word num wd)
    (if (= num 0)
      '()
      (se
        wd
        (repeat-word (- num 1) wd)
      )
    )
  )

  ; Before first and second
  (define (bfs sent)
    (bf (bf sent))
  )

  (cond
    ((equal? '() sent) '())
    ((number? (first sent)) 
      (se
        (repeat-word (first sent) (first (bf sent)))
        (repeat-words (bfs sent))
      )
    )
    (else 
      (se
        (first sent)
        (repeat-words (bf sent))
      )
    )
  )
)

; Exercise 9 - Define same-shape?
(define (same-shape? sent1 sent2)
  (define (wd-length wd)
    (if (equal? wd "")
      0
      (+ 1 (wd-length (bf wd)))
    ) 
  )

  (cond
    ; Check if the sentences are the same length
    ((not (equal? (length sent1) (length sent2))) #f)
    ; If they are the same length (as below also checks for), return true
    ((and (equal? sent1 '()) (equal? sent2 '())) #t)
    ; Check if the words are the same length
    ((not (equal? 
      (wd-length (first sent1))
      (wd-length (first sent2))
    )) #f)
    (else (same-shape? (bf sent1) (bf sent2)))
  )
)
