#lang racket

(require 2htdp/image 2htdp/universe)

(struct pit (snake goos) #:transparent)

(struct snake (dir segs) #:transparent)

(struct posn (x y) #:transparent)

(struct goo (loc expire) #:transparent)

;; -----------------------------------------------------------------------------
;; Constants

;; Tick Rate 
(define TICK-RATE 1/10)

;; Board Size Constants
(define SIZE 30)

;; Snake Constants
(define SEG-SIZE 15)

;; Goo Constants
(define MAX-GOO 5)
(define EXPIRATION-TIME 150)

;; GRAPHICAL BOARD
(define WIDTH-PX  (* SEG-SIZE 30))
(define HEIGHT-PX (* SEG-SIZE 30))

;; Visual constants
(define MT-SCENE (empty-scene WIDTH-PX HEIGHT-PX))
(define GOO-IMG (bitmap "graphics/goo.gif"))
(define SEG-IMG  (bitmap "graphics/body.gif"))
(define HEAD-IMG (bitmap "graphics/head.gif"))

(define HEAD-LEFT-IMG HEAD-IMG)
(define HEAD-DOWN-IMG (rotate 90 HEAD-LEFT-IMG))
(define HEAD-RIGHT-IMG (flip-horizontal HEAD-LEFT-IMG))
(define HEAD-UP-IMG (flip-vertical HEAD-DOWN-IMG))

(define ENDGAME-TEXT-SIZE 15)

(define (start-snake)
  (big-bang (pit (snake "right" (list (posn 1 1)))
                 (list (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)))
            (on-tick next-pit TICK-RATE)
            (on-key direct-snake)
            (to-draw render-pit)
            (stop-when dead? render-end)))

(define (next-pit w)
  (define snake (pit-snake w))
  (define goos (pit-goos w))
  (define goo-to-eat (can-eat snake goos))
  (if goo-to-eat
      (pit (grow snake) (age-goo (eat goos goo-to-eat)))
      (pit (slither snake) (age-goo goos))))

(define (can-eat snake goos)
  (cond
    [(empty? goos) #f]
    [else (if (close? (snake-head snake) (first goos))
              (first goos)
              (can-eat snake (rest goos)))]))

(define (close? s g)
  (posn=? s (goo-loc g)))

(define (eat goos goo-to-eat)
  (cons (fresh-goo) (remove goo-to-eat goos)))

(define (grow sn)
  (snake (snake-dir sn)
         (cons (next-head sn) (sake-segs sn))))

(define (slither sn)
  (snake (snake-dir sn)
         (cons (next-head sn) (all-but-last (snake-segs sn)))))

(define (all-but-last segs)
  (cond
    [(empty? (rest segs)) empty]
    [else (cons (first segs) (all-but-last (rest segs)))]))

(define (next-head sn)
  (define head (snake-head sn))
  (define dir (snake-dir sn))
  (cond
    [(string=? dir "up") (posn-move head 0 -1)]
    [(string=? dir "down") (posn-move head 0 1)]
    [(string=? dir "left") (posn-move head -1 0)]
    [(string=? dir "right") (posn-move head 1 0)]))

(define (posn-move p dx dy)
  (posn (+ (posn-x p) dx)
        (+ (posn-y p) dy)))

(define (age-goo goos)
  (rot (renew goos)))

(define (rot goos)
  (cond
    [(empty? goos) empty]
    [else (cons (decay (first goos)) (rot (rest goos)))]))

(define (renew goos)
  (cond
    [(empty? goos) empty]
    [(rotten? (first goos)) (cons (fresh-goo) (renew (rest goos)))]
    [else (cons (first goos) (renew (rest goos)))]))

(define (rotten? g)
  (zero? (goo-expire g)))

(define (decay g)
  (goo (goo-loc g) (sub1 (goo-expire g))))

(define (fresh-goo)
  (goo (posn (add1 (random (sub1 SIZE)))
             (add1 (random (sub1 SIZE))))
       EXPIRATION-TIME))