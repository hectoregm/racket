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

;; 7. Lexical Scope
(define (rgb-series mk)
  (vc-append
   (series (lambda (sz) (colorize (mk sz) "red")))
   (series (lambda (sz) (colorize (mk sz) "green")))
   (series (lambda (sz) (colorize (mk sz) "blue")))))

;; (rgb-series circle)
;; (rgb-series square)

(define (rgb-maker mk)
  (lambda (sz)
    (vc-append (colorize (mk sz) "red")
               (colorize (mk sz) "green")
               (colorize (mk sz) "blue"))))
;; (series (rgb-maker circle))
;; (series (rgb-maker square))

;; 8. Lists
;; (list "red" "green" "blue")

(define (rainbow p)
  (map (lambda (color)
         (colorize p color))
       (list "red" "orange" "yellow" "green" "blue" "purple")))
;; (rainbow (square 15))

;; Apply example
;; (apply vc-append (rainbow (square 15)))

;; 9. Modules

(require pict/flash)
(require slideshow/code)

;; 10. Macros
(define-syntax pict+code
  (syntax-rules ()
    [(pict+code expr)
     (hc-append 10
                expr
                (code expr))]))
;; (pict+code (circle 10))

;; 11. Objects
(require racket/class
         racket/gui/base)
(define f (new frame% [label "My Art"]
               [width 300]
               [height 300]
               [alignment '(center center)]))
;; (send f show #t)

(define (add-drawing p)
  (let ([drawer (make-pict-drawer p)])
    (new canvas% [parent f]
         [style '(border)]
         [paint-callback (lambda (self dc)
                           (drawer dc 0 0))])))
;; (send f show #t)
;; (add-drawing (pict+code (circle 10)))
;; (add-drawing (colorize (filled-flash 50 30) "yellow"))