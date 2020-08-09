#lang racket
(require 2htdp/universe 2htdp/image)

;; DATA MODEL
;; a gridpoint
;; x :: Int, y :: Int
(struct posn [x y] #:transparent)

;; dir = "up" | "down" | "left" | "right"
;; segs = [posn], nonempty
(struct snake [dir segs] #:transparent)

;; loc :: posn
;; ttl :: Int, number of clock ticks remaining before
;;        goo disappears
(struct goo [loc ttl] #:transparent)

;; the game area
;; sname :: snake, goos :: [posn]
(struct pit [snake goos] #:transparent)

;; (pit
;;  (snake "right" '((posn 0 0) (posn 1 0) (posn 2 0)))
;;  '((goo (posn 5 5) 15) (goo (posn 8 8) 15)))

;; CONSTANTS
(define SIZE 30)
(define GOO_TTL 150)
(define TICK_RATE 1/10)
(define GOO_IMG (circle 10 "solid" "green"))
(define SNAKE_HEAD (circle 10 "solid" "red"))
(define SNAKE_SEG (circle 10 "solid" "blue"))
(define SEG_SIZE 15)
(define BG (empty-scene (* SEG_SIZE SIZE) (* SEG_SIZE SIZE)))

;; HELPERS
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

(define (snake-head s)
  (first (snake-segs s)))

(define (snake-body s)
  (rest (snake-segs s)))

(define (snake-last-seg s)
  (last (snake-segs s)))

(define (snake-changedir s d)
  (snake d (snake-segs s)))

;; CLOCK ACTIONS
(define (fresh-goo)
  (let ([x (add1 (random SIZE))]
        [y (add1 (random SIZE))])
    (goo (posn x y) GOO_TTL)))

(define (goo-rotten? g)
  (zero? (goo-ttl g)))

(define (goo-renew goos)
  (cond [(empty? goos) '()]
        [(goo-rotten? (first goos))
         (cons (fresh-goo) (goo-renew (rest goos)))]
        [else
         (cons (first goos) (goo-renew (rest goos)))]))

(define (goo-rot goos)
  (map
   (lambda (g)
     (goo (goo-loc g)
          (sub1 (goo-ttl g))))
   goos))

(define (age-goos goos)
  (goo-rot (goo-renew goos)))

(define (posn-shift p x-add y-add)
  (let ([x (posn-x p)]
        [y (posn-y p)])
    (posn (+ x x-add) (+ y y-add))))

(define (next-head sn)
  (let ([dir (snake-dir sn)]
        [head (snake-head sn)])
    (cond [(string=? dir "up") (posn-shift head 0 -1)]
          [(string=? dir "down") (posn-shift head 0 1)]
          [(string=? dir "left") (posn-shift head -1 0)]
          [(string=? dir "right") (posn-shift head 1 0)]
          [else head])))

;; exclude empty list check since we defined segs as nonempty
(define (all-but-last segs)
  (cond [(empty? (rest segs)) '()]
        [else (cons (first segs) (all-but-last (rest segs)))]))

(define (slither sn)
  (snake (snake-dir sn)
         (cons (next-head sn)
               (all-but-last (snake-segs sn)))))

(define (grow sn)
  (snake (snake-dir sn)
         (cons (next-head sn)
               (snake-segs sn))))

(define (close? sn g)
  (posn=? (snake-head sn) (goo-loc g)))

(define (can-eat? sn goos)
  (cond [(empty? goos) false]
        [(close? sn (first goos)) (first goos)]
        [else (can-eat? sn (rest goos))]))

(define (eat goos goo-to-eat)
  (cons (fresh-goo) (remove goo-to-eat goos)))

(define (handle-tick w)
  (let* ([goos (pit-goos w)]
         [sn (pit-snake w)]
         [goo-to-eat (can-eat? sn goos)])
    (if goo-to-eat
        (pit (grow sn) (age-goos (eat goos goo-to-eat)))
        (pit (slither sn) (age-goos goos)))))

;; KEY ACTIONS
(define (opposite-dir? d1 d2)
  (cond [(string=? d1 "left") (string=? d2 "right")]
        [(string=? d1 "right") (string=? d2 "left")]
        [(string=? d1 "up") (string=? d2 "down")]
        [(string=? d1 "down") (string=? d2 "up")]
        [else false]))

(define (direction? d)
  (member d '("up" "down" "left" "right")))

(define (change-direction w d)
  (let ([sn (pit-snake w)]
        [goos (pit-goos w)])
    (cond [(and
            (> (length (snake-segs sn)) 1)
            (opposite-dir? d (snake-dir sn)))
           (stop-with w)]
          [else (pit (snake-changedir sn d) goos)])))

(define (handle-key w ke)
  (if (direction? ke)
      (change-direction w ke)
      w))

;; RENDER ACTIONS
(define (render-body sn scene)
  (render-imgs (snake-body sn) SNAKE_SEG scene))

(define (render-snake sn scene)
  (render-img (snake-head sn)
              SNAKE_HEAD
              (render-body sn scene)))

(define (extract-goo-posns goos)
  (cond [(empty? goos) '()]
        [else (cons
               (goo-loc (first goos))
               (extract-goo-posns (rest goos)))]))

(define (render-goos goos scene)
  (render-imgs
   (extract-goo-posns goos)
   GOO_IMG
   scene))

(define (render-imgs ps img scene)
  (cond [(empty? ps) scene]
        [else (render-img
               (first ps)
               img
               (render-imgs (rest ps) img scene))]))

(define (render-img p img scene)
  (place-image
   img
   (* (posn-x p) SEG_SIZE)
   (* (posn-y p) SEG_SIZE)
   scene))

(define (render w)
  (render-snake (pit-snake w)
                (render-goos (pit-goos w) BG)))

;; END CONDITIONS
(define (dead? w)
  (let ([sn (pit-snake w)])
    (or (self-collision? sn)
        (wall-collision? sn))))

(define (wall-collision? sn)
  (let* ([hd (snake-head sn)]
         [hd-x (posn-x hd)]
         [hd-y (posn-y hd)])
    (or (= hd-x 0)
        (= hd-y 0)
        (= hd-y SIZE)
        (= hd-x SIZE))))

(define (self-collision? sn)
  (cons? (member (snake-head sn) (snake-body sn))))

(define (render-end w)
  (overlay (text "Game Over" 15 "black")
           (render w)))

(define (start-snake)
  (big-bang (pit (snake "right" (list (posn 1 1)))
                 (list (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)))
            (on-tick handle-tick TICK_RATE)
            (on-key handle-key)
            (to-draw render)
            (stop-when dead? render-end)))

(start-snake)
