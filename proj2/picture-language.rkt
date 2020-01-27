#lang racket/gui
(require simply-scheme)
(require graphics/graphics)
(open-graphics)
(define vp (open-viewport "A Picture Language" 500 500))

(define draw (draw-viewport vp))
(define (cs) ((clear-viewport vp)))
(define line (draw-line vp))
(provide (all-defined-out))

; Defined by myself
(define (make-vect x y)
  (cons x y)
)

; Defined by myself
(define (xcor-vect v)
  (car v)
)

; Defined by myself
(define (ycor-vect v)
  (cdr v)
)

; Defined by myself
(define (add-vect v1 v2)
  (make-vect
    (+ (xcor-vect v1) (xcor-vect v2))
    (+ (ycor-vect v1) (ycor-vect v2))
  )
)

; Defined by myself
(define (sub-vect v1 v2)
  (make-vect
    (- (xcor-vect v1) (xcor-vect v2))
    (- (ycor-vect v1) (ycor-vect v2))
  )
)

; Defined by myself
(define (scale-vect s v)
  (make-vect
    (* s (xcor-vect v))
    (* s (ycor-vect v))
  )
)

; From Project Spec
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2)
)

; Defined by myself
(define (origin-frame f)
  (car f)
)

; Defined by myself
(define (edge1-frame f)
  (car (cdr f))
)

; Defined by myself
(define (edge2-frame f)
  (car (cdr (cdr f)))
)

; From Project Spec
(define (make-frame-2 origin edge1 edge2)
  (cons origin (cons edge1 edge2))
)

; Defined by myself
(define (origin-frame-2 f)
  (car f)
)

; Defined by myself
(define (edge1-frame-2 f)
  (cdar f)
)

; Defined by myself
(define (edge2-frame-2 f)
  (cddr f)
)

; From Project Spec
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

; Defined by myself
(define (make-segment start end)
  (cons start end)
)
; Defined by myself
(define (start-segment segment)
  (car segment)
)
; Defined by myself
(define (end-segment segment)
  (cdr segment)
)

; From gist, as it has to be specific to graphics package
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (let ((start-coord-map ((frame-coord-map frame) (start-segment segment)))
             (end-coord-map ((frame-coord-map frame) (end-segment segment))))
       (line
        (make-posn (xcor-vect start-coord-map) (ycor-vect start-coord-map))
        (make-posn (xcor-vect end-coord-map) (ycor-vect end-coord-map)))))
     segment-list)))

; Defined by myself
(define x-painter
  (segments->painter
    (list
      (make-segment (make-vect 0 0) (make-vect 1 1))
      (make-segment (make-vect 0 1) (make-vect 1 0))
    )
  )
)

; Defined by myself
(define outline-frame-painter
  (segments->painter
    (list
      (make-segment (make-vect 0 0) (make-vect 0 1))
      (make-segment (make-vect 0 1) (make-vect 1 1))
      (make-segment (make-vect 1 1) (make-vect 1 0))
      (make-segment (make-vect 1 0) (make-vect 0 0))
    )
  )
)

; Defined by myself
(define diamond-painter
  (segments->painter
    (list
      (make-segment (make-vect 0.5 0) (make-vect 0 0.5))
      (make-segment (make-vect 0 0.5 ) (make-vect 0.5 1))
      (make-segment (make-vect 0.5 1) (make-vect 1 0.5))
      (make-segment (make-vect 1 0.5) (make-vect 0.5 0))
    )
  )
)

; Defined by myself
(define wave-painter
  (segments->painter
    (list
      (make-segment (make-vect 0    0.8 ) (make-vect 0.1  0.6 ))
      (make-segment (make-vect 0.1  0.6 ) (make-vect 0.2  0.7 ))
      (make-segment (make-vect 0.2  0.7 ) (make-vect 0.3  0.7 ))
      (make-segment (make-vect 0.3  0.7 ) (make-vect 0.25 0.85))
      (make-segment (make-vect 0.25 0.85) (make-vect 0.3  1   ))
      (make-segment (make-vect 0.4  1   ) (make-vect 0.45 0.85))
      (make-segment (make-vect 0.45 0.85) (make-vect 0.4  0.7 ))
      (make-segment (make-vect 0.4  0.7 ) (make-vect 0.6  0.7 ))
      (make-segment (make-vect 0.6  0.7 ) (make-vect 1    0.3 ))
      (make-segment (make-vect 1    0.2 ) (make-vect 0.4  0.6 ))
      (make-segment (make-vect 0.4  0.6 ) (make-vect 0.5  0   ))
      (make-segment (make-vect 0.4  0   ) (make-vect 0.35 0.4 ))
      (make-segment (make-vect 0.35 0.4 ) (make-vect 0.3  0   ))
      (make-segment (make-vect 0.2  0   ) (make-vect 0.25 0.6 ))
      (make-segment (make-vect 0.25 0.6 ) (make-vect 0.2  0.65))
      (make-segment (make-vect 0.2  0.65) (make-vect 0.1  0.4 ))
      (make-segment (make-vect 0.1  0.4 ) (make-vect 0    0.7 ))
    )
  )
)

; From Project Spec
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

; From Project Spec
(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

; From Project Spec
(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

; Defined by myself
(define (flip-horiz painter)
  (transform-painter
    painter
    (make-vect 1 0)
    (make-vect 0 0)
    (make-vect 1 1)
  )
)

; Defined by myself
(define (rotate-270 painter)
  (transform-painter
    painter
    (make-vect 0 1)
    (make-vect 0 0)
    (make-vect 1 1)
  )
)

; Defined by myself
(define (rotate-180 painter)
  (transform-painter
    painter
    (make-vect 1 1)
    (make-vect 0 1)
    (make-vect 1 0)
  )
)

; Defined by myself
(define (below painter1 painter2)
  (let 
    (
      (paint-top
        (transform-painter
          painter1
          (make-vect 0 0.5)
          (make-vect 1 0.5)
          (make-vect 0 1)
        )
      )
      (paint-bottom
        (transform-painter
          painter2
          (make-vect 0 0)
          (make-vect 1 0)
          (make-vect 0 0.5)
        )
      )
    )
    (lambda (frame)
      (paint-top frame)
      (paint-bottom frame)
    )
  )
)

; Defined by myself
(define (below-2 painter1 painter2)
  (let
    (
      (rotate-90 
        (lambda (painter) 
          (rotate-180 (rotate-270 painter))
        )
      )
    )
    (rotate-270 (beside (rotate-90 painter1)  (rotate-90 painter2)))
  )
)

; Defined by myself
(define (split split1 split2)
  (lambda (painter n)
    (if (= n 0)
      painter
      (let
        (
          [smaller ((split split1 split2) painter (sub1 n))]
        )
        [split1 painter (split2 smaller smaller)]
      )
    )
  )
)

; From Project Spec
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

; Defined by myself
(define (up-split painter n)
  (if (= n 0)
    painter
    (let
      (
        [smaller (up-split painter (sub1 n))]
      )
      [below painter (beside smaller smaller)]
    )
  )
)

; From Project Spec
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

; From Project Spec
(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

; From Gist. 
(define full-frame 
  (make-frame (make-vect 0 500) (make-vect 500 0) (make-vect 0 -500))
)

; Defined by myself
(define wave-painter-mod
  (segments->painter
    (list
      (make-segment (make-vect 0    0.8 ) (make-vect 0.1  0.6 ))
      (make-segment (make-vect 0.1  0.6 ) (make-vect 0.2  0.7 ))
      (make-segment (make-vect 0.2  0.7 ) (make-vect 0.3  0.7 ))
      (make-segment (make-vect 0.3  0.7 ) (make-vect 0.25 0.85))
      (make-segment (make-vect 0.25 0.85) (make-vect 0.3  1   ))
      (make-segment (make-vect 0.4  1   ) (make-vect 0.45 0.85))
      (make-segment (make-vect 0.45 0.85) (make-vect 0.4  0.7 ))
      (make-segment (make-vect 0.4  0.7 ) (make-vect 0.6  0.7 ))
      (make-segment (make-vect 0.6  0.7 ) (make-vect 1    0.3 ))
      (make-segment (make-vect 1    0.2 ) (make-vect 0.4  0.6 ))
      (make-segment (make-vect 0.4  0.6 ) (make-vect 0.5  0   ))
      (make-segment (make-vect 0.4  0   ) (make-vect 0.35 0.4 ))
      (make-segment (make-vect 0.35 0.4 ) (make-vect 0.3  0   ))
      (make-segment (make-vect 0.2  0   ) (make-vect 0.25 0.6 ))
      (make-segment (make-vect 0.25 0.6 ) (make-vect 0.2  0.65))
      (make-segment (make-vect 0.2  0.65) (make-vect 0.1  0.4 ))
      (make-segment (make-vect 0.1  0.4 ) (make-vect 0    0.7 ))
      (make-segment (make-vect 0.29 0.80) (make-vect 0.34 0.75))
      (make-segment (make-vect 0.34 0.75) (make-vect 0.39 0.80))

    )
  )
)

; Defined by myself
(define (corner-split-mod painter n)
  (lambda (frame)
    (let
      (
        (iden painter)
        (tl painter)
        (br painter)
        (tr (corner-split-mod painter (sub1 n)))
      )
      (if (zero? n)
        (iden frame)
        ((beside
          (below iden tl)
          (below br tr)
        ) frame)
      )
    )
  )
)

; From Project Spec
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below top bottom))))

(define (square-limit-mod painter n)
  (let 
    (
      (combine4
        (square-of-four identity flip-horiz flip-vert rotate-180)
      )
    )
    (combine4 (corner-split painter n))
  )
)

((square-limit-mod wave-painter-mod 4) full-frame)