#lang racket

(define (my-map f xs)
  (cond [(empty? xs) xs]
        [else (cons
               (f (first xs))
               (my-map f (rest xs)))]))

;;(my-map (lambda (x) (+ x 10)) '(10 20 30))

(define (my-filter f xs)
  (cond [(empty? xs) xs]
        [(f (first xs))
         (cons (first xs)
               (my-filter f (rest xs)))]
        [else (my-filter f (rest xs))]))

;;(my-filter (lambda (x) (zero? (remainder x 2))) '(10 21 31 40 55))

(define (my-ormap f xs)
  (cond [(empty? xs) false]
        [else (or (f (first xs))
                  (my-ormap f (rest xs)))]))

;;(my-ormap even? '(11 13 45 12 17))

(define (my-andmap f xs)
  (cond [(empty? xs) true]
        [else (and (f (first xs))
                   (my-andmap f (rest xs)))]))


(define (my-foldl f acc xs)
  (cond [(empty? xs) acc]
        [else (my-foldl f
                        (f acc (first xs))
                        (rest xs))]))

(define (my-foldr f acc xs)
  (cond [(empty? xs) acc]
        [else (f
               (my-foldr f acc (rest xs))
               (first xs))]))


(define (my-build-list f n)
  (define (go k)
    (cond [(= k n) '()]
          [else (cons (f k) (go (add1 k)))]))
  (go 0))

;; (my-build-list add1 5)
;; (my-build-list (lambda (x) (*  x x)) 5)
