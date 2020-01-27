#lang racket
(require simply-scheme)
(provide (all-defined-out))

(define (func-pair pair)
  ((car pair) (cdr pair))
)