#lang racket

(define (sum-prod)
  (for/fold ([sums 0]
             [prods 1])
            ([a '(1 2 3 4 5)])
    (values (+ a sums) (* a prods))))

;; (define-values (x y) (values 2 3))
;; (define-values (su pr) (sum-prod))


;; (for/list ([i '(1 2 3 4 5)])
;;   (* i i))

;; (for/list ([i '(1 2 3 4 5)] #:when (even? i))
;;   (* i i))

;; (for* ([i '(1 2 3)]
;;        [j '(10 20 30)]
;;        [k '(100 200 300)])
;;   (display (list i j k)))

;; (for*/list ([i '("foo" "bar" "baz")]
;;             [j '("fizz" "buzz" "kuzz")])
;;   (string-append i j))


;; in-range produces iterable sream [0,n)
(define (son-name-generator son-count)
  (for ([i (in-range son-count)])
    (display
     (string-append "you are son #"
                    (number->string (add1 i))
                    "\n"))))

;;(son-name-generator 3)


