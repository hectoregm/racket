#lang plai

(define the-receiver (box 'dummy-value))
(define receiver-prompt (box 'dummy-value))

(define (web-display n)
  (printf "Web output: ~a~n" n))

(define (web-read/k p k)
  (begin
    (set-box! receiver-prompt p)
    (set-box! the-receiver k)
    ((error 'web-read/k "run (resume) to enter number and simulate clicking Submit"))))

(define (resume)
  (begin
    (display (unbox receiver-prompt))
    ((unbox the-receiver) (read))))

(define receiver (lambda (x)
                   (web-read/k "Second number"
                               (lambda (y)
                                 (web-display (+ x y))))))

(define (tally-old item-list)
  (if (empty? item-list)
      0
      (+ (web-read (generate-item-cost-prompt (first))))))