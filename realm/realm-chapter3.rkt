#lang racket

(struct student (name id# dorm) #:transparent)

(define freshman1 (student 'Joe 1234 'NewHall))

