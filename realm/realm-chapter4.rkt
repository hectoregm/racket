#lang racket

(struct student (name id# dorm) #:transparent)

(define freshman1 (student 'Joe 1234 'NewHall))

(define (my-length a-list)
  (cond
    [(empty? a-list) 0]
    [else (add1 (my-length (rest a-list)))]))

(struct point (x y) #:transparent)