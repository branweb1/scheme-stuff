#lang racket
(require 2htdp/universe 2htdp/image)

(struct posn [x y] #:transparent)
(struct state [loc dir])
(define HEIGHT 400)
(define WIDTH 400)
(define SPEED 8)
(define INIT_X (/ WIDTH 2))
(define INIT_Y (/ HEIGHT 2))
(define UFO
  (overlay/align
   "center" "bottom"
   (circle 15 "solid" "green")
   (ellipse 70 20 "solid" "green")))

(define (render-smoke op)
  (op (circle 10 "solid" "gray")
      (circle 10 "solid" "gray")
      (circle 10 "solid" "gray")))

(define (render-ufo-and-smoke dir)
  (cond [(string=? dir "up")
         (overlay/offset UFO 0 (image-height UFO) (render-smoke above))]
        [(string=? dir "down")
         (overlay/offset UFO 0 (- 0 (image-height UFO)) (render-smoke above))]
        [(string=? dir "left")
         (overlay/offset UFO (image-width UFO) 0 (render-smoke beside))]
        [(string=? dir "right")
         (overlay/offset UFO (- 0 (image-width UFO)) 0 (render-smoke beside))]
        [else UFO]))

(define (render s)
  (let ([x (posn-x (state-loc s))]
        [y (posn-y (state-loc s))])
    (place-image (render-ufo-and-smoke (state-dir s)) x y
                 (empty-scene WIDTH HEIGHT))))

(define (calc-coord c test? op)
  (if test? (op c SPEED) c))

(define (handle-key s key)
  (let ([x (posn-x (state-loc s))]
        [y (posn-y (state-loc s))])
    (cond [(key=? key "up")
           (state (posn x (calc-coord y (> y 0) -)) "up")]
          [(key=? key "down")
           (state (posn x (calc-coord y (< y HEIGHT) +)) "down")]
          [(key=? key "left")
           (state (posn (calc-coord x (> x 0) -) y) "left")]
          [(key=? key "right")
           (state (posn (calc-coord x (< x WIDTH) +) y) "right")]
          [else s])))

(define (main)
  (big-bang (state (posn INIT_X INIT_Y) "rest")
            [on-key handle-key]
            [to-draw render]))

(main)
