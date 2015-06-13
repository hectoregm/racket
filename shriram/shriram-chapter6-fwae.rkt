#lang plai

(print-only-errors true)

(define-type FWAE
  [num (n number?)]
  [add (lhs FWAE?)
       (rhs FWAE?)]
  [sub (lhs FWAE?)
       (rhs FWAE?)]
  [mult (lhs FWAE?)
        (rhs FWAE?)]
  [with (name symbol?)
        (named-expr FWAE?)
        (body FWAE?)]
  [id (name symbol?)]
  [fun (param symbol?)
       (body FWAE?)]
  [app (fun-expr FWAE?)
       (arg-expr FWAE?)])

(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    [(list? sexp)
     (case (first sexp)
       [(+) (add (parse (second sexp))
                 (parse (third sexp)))]
       [(-) (sub (parse (second sexp))
                 (parse (third sexp)))]
       [(*) (mult (parse (second sexp))
                 (parse (third sexp)))]
       [(with) (with (first (second sexp))
                     (parse (second (second sexp)))
                     (parse (third sexp)))]
       [(fun) (fun (first (second sexp))
                   (parse (third sexp)))]
       [else (app (parse (first sexp))
                  (parse (second sexp)))])]))

(define (subst expr sub-id val)
  (type-case FWAE expr
    [num (n) expr]
    [add (l r) (add (subst l sub-id val)
                    (subst r sub-id val))]
    [sub (l r) (sub (subst l sub-id val)
                    (subst r sub-id val))]
    [mult (l r) (mult (subst l sub-id val)
                      (subst r sub-id val))]
    [with (bound-id named-expr bound-body)
          (if (symbol=? bound-id sub-id)
              (with bound-id
                    (subst named-expr sub-id val)
                    bound-body)
              (with bound-id
                    (subst named-expr sub-id val)
                    (subst bound-body sub-id val)))]
    [id (v) (if (symbol=? v sub-id) val expr)]
    [fun (p b) (fun p (subst b sub-id val))]
    [app (f e) (app (subst f sub-id val)
                    (subst e sub-id val))]))


;; calc : FWAE -> number
(define (calc expr)
  (type-case FWAE expr
    [num (n) n]
    [add (l r) (+ (calc l) (calc r))]
    [sub (l r) (- (calc l) (calc r))]
    [mult (l r) (* (calc l) (calc r))]
    [with (bound-id named-expr bound-body)
          (calc (subst bound-body
                      bound-id
                      (num (calc named-expr))))]
    [id (v) (error 'calc "free identifier")]
    [else empty]))

(test (calc (parse '3)) 3)
(test (calc (parse '{+ 3 4})) 7)
(test (calc (parse '{+ {- 3 4} 7})) 6)
(test (calc (parse '{with {x {+ 5 5}} {+ x x}})) 20)
(test (calc (parse '{with {x 5} {+ x x}})) 10)
(test (calc (parse '{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}})) 14)
(test (calc (parse '{with {x 5} {with {y {- x 3}} {+ y y}}})) 4)
(test (calc (parse '{with {x 5} {+ x {with {x 3} 10}}})) 15)
(test (calc (parse '{with {x 5} {+ x {with {x 3} x}}})) 8)
(test (calc (parse '{with {x 5} {+ x {with {y 3} x}}})) 10)
(test (calc (parse '{with {x 5} {with {y x} y}})) 5)
(test (calc (parse '{with {x 5} {with {x x} x}})) 5)

;; add-numbers FWAE FWAE -> FWAE
;; takes two num subtypes returns the sum in a new sum.
(define (add-numbers numa numb)
  (num (+ (num-n numa)
          (num-n numb))))

(define (rest-numbers numa numb)
  (num (- (num-n numa)
          (num-n numb))))

(define (mult-numbers numa numb)
  (num (* (num-n numa)
          (num-n numb))))

;; interp : FWAE -> FWAE
;; evaluates FWAE expressions by reducing them to their corresponding values
;; return values are either num or fun
(define (interp expr)
  (type-case FWAE expr
    [num (n) expr]
    [add (l r) (add-numbers (interp l) (interp r))]
    [sub (l r) (rest-numbers (interp l) (interp r))]
    [mult (l r) (mult-numbers (interp l) (interp r))]
    [with (bound-id named-expr bound-body)
          (interp (subst bound-body
                         bound-id
                         (interp named-expr)))]
    [id (v) (error 'interp (string-append (symbol->string v) "  free identifier"))]
    [fun (bound-id bound-body)
         expr]
    [app (fun-expr arg-expr)
         (local ([define fun-val (interp fun-expr)])
           (interp (subst (fun-body fun-val)
                          (fun-param fun-val)
                          (interp arg-expr))))]))
(test (interp (parse '3)) (num 3))
(test (interp (parse '{+ 3 4})) (num 7))
(test (interp (parse '{+ {- 3 4} 7})) (num 6))
(test (interp (parse '{with {x {+ 5 5}} {+ x x}})) (num 20))
(test (interp (parse '{with {x 5} {+ x x}})) (num 10))
(test (interp (parse '{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}})) (num 14))
(test (interp (parse '{with {x 5} {with {y {- x 3}} {+ y y}}})) (num 4))
(test (interp (parse '{with {x 5} {+ x {with {x 3} 10}}})) (num 15))
(test (interp (parse '{with {x 5} {+ x {with {x 3} x}}})) (num 8))
(test (interp (parse '{with {x 5} {+ x {with {y 3} x}}})) (num 10))
(test (interp (parse '{with {x 5} {with {y x} y}})) (num 5))
(test (interp (parse '{with {x 5} {with {x x} x}})) (num 5))
(test (interp (parse '{{fun {x} x} 3})) (num 3))
(test (interp (parse '{{{fun {x} x} {fun {x} {+ x 5}}} 3})) (num 8))
(test (interp (parse '{with {x 3} {fun {y} {+ x y}}})) (fun 'y (add (num 3) (id 'y))))
(test (interp (parse '{with {x 10} {{fun {y} {+ y x}} {+ 5 x}}})) (num 25))
 (interp (parse '{with {x {+ 1 1}} {with {y 3} {with {z {* 1 4}} {with {foo {fun {a} z}} {foo 0}}}}}))