#lang racket

(require 2htdp/image 2htdp/universe)

(struct pit (snake goos))

(struct snake (dir segs))

(struct posn (x y))

(struct goo (loc expire))

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