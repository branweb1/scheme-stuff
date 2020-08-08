#lang racket

(define upper 100)
(define lower 1)

(define (guess)
  (quotient (+ upper lower) 2))

(define (smaller)
  (set! upper (max lower (sub1 (guess))))
  (guess))

(define (bigger)
  (set! lower (min upper (add1 (guess))))
  (guess))

(define (start m n)
  (set! lower (min m n))
  (set! upper (max m n))
  (guess))
