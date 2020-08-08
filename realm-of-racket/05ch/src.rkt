#lang racket
(require 2htdp/universe 2htdp/image)

(struct bounds [lower upper])
(define TEXT_SIZE 16)
(define HELP1
  (text "↑ if number is higher, ↓ if it's lower"
        TEXT_SIZE
        "blue"))
(define HELP2
  (text "= if number was guessed; q to quit"
        TEXT_SIZE
        "blue"))
(define COLOR "red")
(define WIDTH 400)
(define HEIGHT 350)
(define TEXT_X (/ WIDTH 2))

(define BG
  (place-image/align
   HELP1 TEXT_X (/ HEIGHT 20) "center" "top"
   (place-image/align
    HELP2 TEXT_X (- HEIGHT (image-height HELP2)) "center" "center"
    (empty-scene WIDTH HEIGHT))))

(define (guess state)
  (quotient
   (+ (bounds-upper state) (bounds-lower state))
   2))

(define (higher state)
  (bounds (min (bounds-upper state) (add1 (guess state)))
          (bounds-upper state)))

(define (lower state)
  (bounds (bounds-lower state)
          (max (bounds-lower state) (sub1 (guess state)))))


(define (handle-draw state)
  (overlay
   (text (number->string (guess state))
         TEXT_SIZE
         COLOR)
   BG))

(define (render-last-scene state)
  (overlay
   (text
    (string-append  "End. It was " (number->string (guess state)) ".")
    TEXT_SIZE COLOR)
   BG))

(define (single? state)
  (= (bounds-upper state) (bounds-lower state)))

(define (handle-key state key)
  (cond [(key=? key "up") (higher state)]
        [(key=? key "down") (lower state)]
        [(key=? key "q") (stop-with state)]
        [(key=? key "=") (stop-with state)]
        [else state]))

(define (start m n)
  (big-bang (bounds (min m n) (max m n))
            [on-key handle-key]
            [to-draw handle-draw]
            [stop-when single? render-last-scene]))

(start 1 100)
