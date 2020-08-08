#lang racket

;; HELPERS
(define (atom? x)
  (and (not (empty? x))
       (not (pair? x))))

;; FIRST COMMANDMENT
;; When recurring on a number, ask two questions: is it zero and else.

(define (add m n)
  (cond [(zero? m) n]
        [else (add (- m 1) (+ n 1))]))
;;(add 5 6)

;; When recurring on a list, ask two questions: is it empty and else
(define (len xs)
  (cond [(empty? xs) 0]
        [else (+ 1 (len (rest xs)))]))
;;(len '(1 2 3))

;; When recurring on a nested list, ask three questions: is it empty, is element we can get at an atom, and else
(define (len* xs)
  (cond [(empty? xs) 0]
        [(atom? (first xs))
         (+ 1 (len* (rest xs)))]
        [else (+ (len* (first xs))
                 (len* (rest xs)))]))
;;(len* '(a b (c d) e (f)))

;; Recursion that follows the above patterns is called natural recursion

;; SECOND COMMANDMENT
;; Build lists with cons
(define (rember a xs)
  (cond [(empty? xs) '()]
        [(equal? a (first xs))
         (rember a (rest xs))]
        [else (cons (first xs)
                    (rember a (rest xs)))]))

;;(rember 'dog '(cat pig dog sheep dog cow))

;; Use the commandments to break recursion problems down into simple yes/no questions. This lets you avoid exploading your brain by trying to think of a big problem all at once. E.g. with "rember":
;; - is list empty? If yes, can't remove something from an empty list, so just return it. If no, then what?
;; - I can get at the first element, so what can I do with it? Check if it equals a.
;; - If yes, drop it and keep going
;; - If no, save it in a cons call and keep going

;; THIRD COMMANDMENT
;; When building up a list, think about the first element of the list you want to build. Figure out how to make it, then cons it to the recursive call. It can be helpful to write a call the function before defining it.

(define (firsts xxs)
  (cond [(empty? xxs) '()]
        [else
         (cons (first (first xxs))
               (firsts (rest xxs)))]))

;; (firsts '((dog cat mouse)
;;           (pig sheep moose)
;;           (deer racoon mole)))

;; FOURTH COMMANDMENT
;; When recurring, modify the argument on each call to work towards your base case. With lists, use (rest xs). On a number, use (- n 1).

;; FIFTH COMMANDMENT
;; When building up a value, use its identity as your base case. For lists, use '(). For a sum, use 0. For a product, use 1.

(define (sum xs)
  (cond [(empty? xs) 0]
        [else (+ (first xs) (sum (rest xs)))]))

;;(sum '(2 4 6))

(define (prod xs)
  (cond [(empty? xs) 1]
        [else (* (first xs) (prod (rest xs)))]))

;;(prod '(2 2 2 2))

;; SIXTH COMMANDMENT
;; Write the function and ensure it is correct. Only then do you simplify

;; SEVENTH COMMANDMENT
;; Recur on subparts of the same nature. If a function param is of type list, recur on a sublist. Don't change the type between recursive calls in other words.

;; EIGHTH COMMANDMENT
;; Use helper functions to decouple your program from the particular representation of data. Two is the data. 2 or II or (() ()) is the representation. If we want to change it, we only need to change the helper functions and the rest of your program will "just work."

(define (operator expr)
  (first expr))

(define (1st-subexpr expr)
  (second expr))

(define (2nd-subexpr expr)
  (third expr))

(define (a-to-f x)
  (cond [(equal? x '+) +]
        [(equal? x '*) *]
        [else expt]))

(define (value expr)
  (cond [(atom? expr) expr]
        [else ((a-to-f (operator expr))
               (1st-subexpr expr)
               (2nd-subexpr expr))]))

;(value '(+ 2 2))
;(value '(pow 2 3))

;; Beware of shadows
;; Instead of using arab numerals to represnt the concept "two," we use lists of empty lists--e.g. this is three: (() () ()). Now these and the arab numerals shadow one another--they are two representations of the same concept existing simultaneously in our program. This can have strange consequences--see the call to lat?, which determines if its arg is a list of atoms. We have a function returning two different values for what is really the same logical thing.

(define (convert-num xs)
  (cond [(empty? xs) 0]
        [else (+ 1 (convert-num (rest xs)))]))

(define (alt-sum xxs)
  (cond [(empty? xxs) 0]
        [else
         (+ (convert-num (first xxs))
            (alt-sum (rest xxs)))]))

;; (alt-sum '((() ())
;;            (() ())))

(define (lat? xs)
  (cond [(empty? xs) #t]
        [else (and (atom? (first xs))
                   (lat? (rest xs)))]))

;; (lat? '(2 2))
;; (lat? '((() ()) (() ())))


;; NINTH COMMANDMENT
;; Abstract common patterns aka DRY. Can use higher order functions, currying and partial application, or just plain old pulling a common chunk of code out of two functions and wrapping it in its own function

;; TENTH COMMANDMENT
;; build functions that collect values

(define (my-reduce reducer ident xs)
  (cond [(empty? xs) ident]
        [else
         (my-reduce
          reducer
          (reducer ident (first xs))
          (rest xs))]))

(my-reduce (lambda (acc a) (+ acc a)) 0 '(10 20 30))

