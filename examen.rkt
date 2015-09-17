#lang plai

(define (misterio x)
  (cond
    [(empty? x) empty]
    [(empty? (rest x)) x]
    [else (let [(i (first x))]
            (if (equal? i (first (rest x)))
                (misterio (rest x))
                (cons i (misterio (rest x)))))]))


(test (misterio '()) '())
(test (misterio '(1)) '(1))
(test (misterio '(1 1)) '(1))
(test (misterio '(1 1 2)) '(1 2))
(test (misterio '(1 1 2 1)) '(1 2 1))
(test (misterio '(1 1 1 2 2 1)) '(1 2 1))

(define misterio2
  (lambda (x y z)
    (cond
      [(cons? y) (cons (misterio2 x (car y) z)
                       (misterio2 x (cdr y) z))]
      [(eq? x y) z]
      [else y])))
(test (misterio2 'x '() 'z) '())
(test (misterio2 'x 'x 'z) 'z)
(test (misterio2 'x 'y 'z) 'y)
(test (misterio2 'x '(a b c d) 'z) '(a b c d))
(test (misterio2 'x '(x y w z x) 'z) '(z y w z z))

(define misterio3
  (lambda (x y)
    (cond
      [(cons? y) (if (eq? (misterio3 x (car y)) x)
                     x
                     (misterio3 x (cdr y)))]
      [(eq? x y) x]
      [else '()])))
(test (misterio3 'x '()) '())
(test (misterio3 'x '(x)) 'x)
(test (misterio3 'x '(x y z)) 'x)
(test (misterio3 'x '(w y z)) '())
(test (misterio3 'x '(w y z x)) 'x)