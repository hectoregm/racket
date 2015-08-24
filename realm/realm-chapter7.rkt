#lang racket

(define (my-map func lst)
  (cond
    [(empty? lst) empty]
    [else (cons (func (first lst))
                (my-map func (rest lst)))]))


(define (my-filter pred lst)
  (cond
    [(empty? lst) empty]
    [(pred (first lst)) (cons (first lst) (my-filter (rest lst)))]
    [else (my-filter (rest lst))]))

(define (d/dx fun)
  (define delta (/ 1 100000))
  (lambda (x)
    (/ (- (fun (+ x delta)) (fun (- x delta))) 2 delta)))