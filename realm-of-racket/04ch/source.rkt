#lang racket
(require 2htdp/universe 2htdp/image)

(define HEIGHT 400)
(define WIDTH 275)
(define UFO (ellipse 60 20 "solid" "blue"))

(define (handle-state n)
  (+ n 3))

(define (draw-state n)
  (place-image
   UFO (/ WIDTH 2) n
   (empty-scene WIDTH HEIGHT)))

(define (handle-stop n)
  (>= n (- HEIGHT (/ (image-height UFO) 2))))

(big-bang 0
          (on-tick handle-state)
          (to-draw draw-state)
          (stop-when handle-stop))
