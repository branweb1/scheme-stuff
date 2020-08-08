#lang racket
(require 2htdp/universe 2htdp/image)


(define TRAIN-IMG
  (bitmap/file (build-path (current-directory) "train.png")))
(define WIDTH 400)
(define HEIGHT 300)

(define (render n)
  (place-image/align
   (text "q to quit" 16 "blue") (/ WIDTH 2) (/ HEIGHT 20) "center" "top"
   (place-image/align
    TRAIN-IMG  n  HEIGHT  "right" "bottom"
    (empty-scene WIDTH HEIGHT))))

(define (handle-tick n)
  (if (<= n (- 50 (image-width TRAIN-IMG)))
      (+ (image-width TRAIN-IMG) WIDTH)
      (- n 3)))

(define (handle-key n key)
  (cond [(key=? key "q") (stop-with n)]
        [else n]))

(define (stop? n)
  false)

(define (main)
  (big-bang WIDTH
            [on-tick handle-tick]
            [to-draw render]
            [on-key handle-key]
            [stop-when stop?]))

(main)
