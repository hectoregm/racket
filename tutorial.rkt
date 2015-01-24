#lang slideshow

;; 4. Definitions

(define c (circle 10))
(define r (rectangle 10 20))

(define (square n)
  (filled-rectangle n n))

;; 5. Local Binding

(define (four p)
  (define two-p (hc-append p p))
  (vc-append two-p two-p))


;; let form binds many identifiers at the same time, so the bindings
;; cannot refer to each other
(define (checker p1 p2)
  (let ((p12 (hc-append p1 p2))
        (p21 (hc-append p2 p1)))
    (vc-append p12 p21)))

;; let* form allow later bindings to use earlier bindings
(define (checkerboard p)
  (let* ([rp (colorize p "red")]
         [bp (colorize p "black")]
         [c (checker rp bp)]
         [c4 (four c)])
    (four c4)))

;; 6. Functions are Values
(define (series mk)
  (hc-append 4 (mk 5) (mk 10) (mk 20)))

(define series-lambda
  (lambda (mk)
    (hc-append 4 (mk 5) (mk 10) (mk 20))))
;; (series-lambda (lambda (size) (checkerboard (square size))))