#lang racket
(require 2htdp/image 2htdp/universe)
(require rackunit)

;; DATA MODEL
;; mutable keyword generates setter function in format set-<struct>-<attr>!
(struct orc-world [player monsters attack# target] #:transparent #:mutable)
(struct player [health strength agility] #:transparent #:mutable)
(struct monster ([health #:mutable]) #:transparent)
(struct orc monster [club] #:transparent)
(struct hydra monster [] #:transparent)
(struct slime monster [sliminess] #:transparent)
(struct brigand monster [] #:transparent)

;; CONSTANTS
;; player attrs
(define MAX_HEALTH 35)
(define MAX_AGILITY 35)
(define MAX_STRENGTH 35)

;; monster attrs
(define MONSTER_HEALTH 9)
(define CLUB_STRENGTH 8)
(define SLIMINESS 5)
(define MONSTER# 12)
(define STRENGTH_DAMAGE -4)
(define AGILITY_DAMAGE -3)
(define HEALTH_DAMAGE -2)

;; other
(define ATTACK# 4)
(define HEAL_AMT 8)
(define STAB_DAMAGE 2)
(define FLAIL_DAMAGE 3)

;; HELPERS
;; [1, n]
(define (random+ n)
  (add1 (random n)))

;; [-1, -n]
(define (random- n)
  (- (random+ n)))

(define (random-quotient dividend divisor)
  (let ([res (quotient dividend divisor)])
    (if (> 0 res) 0 (random+ (add1 res)))))

(define (interval+ lvl delta maxlvl)
  (let ([newlvl (+ lvl delta)])
    (cond [(< newlvl 0) 0]
          [(> newlvl maxlvl) maxlvl]
          [else newlvl])))

(define (interval- lvl delta)
  (let ([newlvl (- lvl delta)])
    (if (< newlvl 0) 0 newlvl)))

;; MUTATORS
(define (player-update! getter setter max-val)
  (lambda (player delta)
    (let ([new-val (interval+ (getter player) delta max-val)])
      (setter player new-val))))

(define player-health+
  (player-update! player-health set-player-health! MAX_HEALTH))

(define player-agility+
  (player-update! player-agility set-player-agility! MAX_AGILITY))

(define player-strength+
  (player-update! player-strength set-player-strength! MAX_STRENGTH))

;; STATE SETUP
(define (init-player)
  (player MAX_HEALTH MAX_STRENGTH MAX_AGILITY))

(define (init-monsters)
  (build-list
   MONSTER#
   (lambda (_)
     (let ([health (random+ MONSTER_HEALTH)])
       (case (random 4)
         [(0) (orc health (random+ CLUB_STRENGTH))]
         [(1) (hydra health)]
         [(2) (slime health (random+ SLIMINESS))]
         [(3) (brigand health)])))))

(define (random-attack# player)
  (random-quotient (player-agility player) ATTACK#))

(define (init-orc-world)
  (let ([player (init-player)])
    (orc-world
     player
     (init-monsters)
     (random-attack# player)
     0)))

;; ACTIONS
(define (decrease-attack# w)
  (set-orc-world-attack#! w (sub1 (orc-world-attack# w))))

(define (damage-monster target amt)
  (set-monster-health!
   target
   (interval- (monster-health target) amt)))

(define (end-turn w)
  (set-orc-world-attack#! w 0))

(define (is-alive? m)
  (> (monster-health m) 0))

(define (current-target w)
  (list-ref (orc-world-monsters w) (orc-world-target w)))

(define (heal w)
  (decrease-attack# w)
  (player-health+ (orc-world-player w) HEAL_AMT))

(define (stab w)
  (let* ([target-idx (orc-world-target w)]
        [target (list-ref (orc-world-monsters w) target-idx)]
        [amt (random-quotient
              (player-strength (orc-world-player w))
              STAB_DAMAGE)])
    (decrease-attack# w)
    (damage-monster target amt)))

(define (flail w)
  (let* ([player (orc-world-player w)]
         [alive (filter is-alive? (orc-world-monsters w))]
         [pick# (min
                 (random-quotient (player-strength player) FLAIL_DAMAGE)
                 (length alive))]
         [monsters-still-living (take alive pick#)]
         [current-target (current-target w)]
         [monsters-to-attack (cons current-target monsters-still-living)])
    (decrease-attack# w)
    (for-each
     (lambda (m) (damage-monster m 1))
     monsters-to-attack)))

(define (player-acts-on-monster w k)
  (cond [(key=? k "s") (stab w)]
        [(key=? k "f") (flail w)]
        [(key=? k "h") (heal w)]
        [(key=? k "e") (end-turn w)]
        [(key=? k "n") (init-orc-world)]
        [(key=? k "up") (move-target w add1)]
        [(key=? k "down") (move-target w sub1)])
  (monster-attack-turn w)
  w)

(define (monster-attack-turn w)
  (when (zero? (orc-world-attack# w))
    (let ([player (orc-world-player w)]
          [monsters (orc-world-monsters w)])
      (all-monsters-attack player monsters)
      (set-orc-world-attack#! w (random-attack# player)))))

(define (all-monsters-attack p monsters)
  (define (monster-attack m)
    (cond [(orc? m)
           (player-health+ p (random- (orc-club m)))]
          [(hydra? m)
           (player-health+ p (random- (monster-health m)))]
          [(slime? m)
           (player-agility+ p (random- (slime-sliminess m)))
           (player-health+ p -1)]
          [(brigand? m)
           (case (random 3)
             [(0) (player-health+ p HEALTH_DAMAGE)]
             [(1) (player-agility+ p AGILITY_DAMAGE)]
             [(2) (player-strength+ p STRENGTH_DAMAGE)])]))
  (let ([live-monsters (filter is-alive? monsters)])
    (for-each monster-attack live-monsters)))

(define (move-target w f)
  (let* ([current-target-idx (orc-world-target w)]
         [monster-count (length (orc-world-monsters w))]
         [new-target (modulo (f current-target-idx) monster-count)])
    (set-orc-world-target! w new-target)))

(define (all-dead? monsters)
  (not (ormap is-alive? monsters)))

(define (player-dead? p)
  (let ([health (player-health p)]
        [agility (player-agility p)]
        [strength (player-strength p)])
    (ormap (lambda (attr) (zero? attr)) (list health agility strength))))

(define (win? w)
  (all-dead? (orc-world-monsters w)))

(define (lose? w)
  (player-dead? (orc-world-player w)))

(define (game-over? w)
  (or (win? w) (lose? w)))

;; RENDER
(define TEXT_SIZE 16)
(define TEXT_COLOR "blue")
(define TEXT_COLOR_SELECTED "red")

(define (monster->text m)
  (let ([name
         (cond [(orc? m) "orc"]
               [(hydra? m) "hydra"]
               [(slime? m) "slime"]
               [(brigand? m) "brigand"])])
    (text
     (string-append name " " (number->string (monster-health m)))
     TEXT_SIZE
     TEXT_COLOR_SELECTED)))

(define (monsters->text monsters)
  (foldr
   (lambda (a acc) (above acc (monster->text a)))
   (text "MONSTERS" TEXT_SIZE TEXT_COLOR)
   monsters))

(define (player->text p)
  (above (text "PLAYER" TEXT_SIZE TEXT_COLOR)
         (text
          (string-append
           "H:"
           (number->string (player-health p))
           " S:"
           (number->string (player-strength p))
           " A:"
           (number->string (player-agility p)))
          TEXT_SIZE
          TEXT_COLOR)))

(define (render-instructions)
  (above
   (text "s - stab | f - flail | h - heal | e - end turn"
         TEXT_SIZE
         TEXT_COLOR)
   (text "use arrow keys to change target"
         TEXT_SIZE
         TEXT_COLOR)
   (text "n to restart game"
         TEXT_SIZE
         TEXT_COLOR)))

(define (render w)
  (place-image/align
   (above
    (beside (monsters->text (orc-world-monsters w)) (player->text (orc-world-player w)))
    (render-instructions)
    (text (string-append "current target:" (number->string (orc-world-target w))) TEXT_SIZE TEXT_COLOR)
    (text (string-append "attacks:" (number->string (orc-world-attack# w))) TEXT_SIZE TEXT_COLOR))
   250 10
   "center"
   "top"
   (empty-scene 500 500)))


(define (render-end w)
  (let ([msg (if (win? w) "win" "lose")])
    (overlay (text msg TEXT_SIZE TEXT_COLOR)
             (render w))))

(define (start)
  (big-bang (init-orc-world)
            [on-key player-acts-on-monster]
            [to-draw render]
            [stop-when game-over? render-end]))

(start)

;; TESTS
(check-equal? (interval+ 12 20 30) 30 "Doesn't pass max")
(check-equal? (interval+ 9 20 30) 29 "Incs health")
(check-equal? (interval+ 10 -20 30) 0 "Doesn't pass zero")

(check-equal? (let ([p (player 10 10 10)])
                (player-health+ p 10)
                p)
              (player 20 10 10) "Increases player health")

(check-equal? (let ([p (player 10 10 10)])
                (player-health+ p -10)
                p)
              (player 0 10 10) "Decreases player health")

