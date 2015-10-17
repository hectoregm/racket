#lang plai

(print-only-errors true)

(define-type FAES
  [numS (n number?)]
  [addS (lhs FAES?)
        (rhs FAES?)]
  [subS (lhs FAES?)
        (rhs FAES?)]
  [withS (name symbol?)
         (named-expr FAES?)
         (body FAES?)]
  [idS (name symbol?)]
  [funS (param symbol?)
        (body FAES?)]
  [appS (fun-expr FAES?)
        (arg-expr FAES?)])

(define-type FAE
  [num (n number?)]
  [add (lhs FAE?)
       (rhs FAE?)]
  [sub (lhs FAE?)
       (rhs FAE?)]
  [id (name symbol?)]
  [fun (param symbol?)
       (body FAE?)]
  [app (fun-expr FAE?)
       (arg-expr FAE?)])

(define (parse sexp)
  (cond
    [(number? sexp) (numS sexp)]
    [(symbol? sexp) (idS sexp)]
    [(list? sexp)
     (case (first sexp)
       [(+) (addS (parse (second sexp))
                 (parse (third sexp)))]
       [(-) (subS (parse (second sexp))
                 (parse (third sexp)))]
       [(with) (withS (first (second sexp))
                     (parse (second (second sexp)))
                     (parse (third sexp)))]
       [(fun) (funS (first (second sexp))
                   (parse (third sexp)))]
       [else (appS (parse (first sexp))
                  (parse (second sexp)))])]))

(define (desugar expr)
  (type-case FAES expr
    [numS (n) (num n)]
    [addS (l r) (add (desugar l)
                     (desugar r))]
    [subS (l r) (sub (desugar l)
                     (desugar r))]
    [withS (id named body) (app (fun id (desugar body))
                                (desugar named))]
    [idS (s) (id s)]
    [funS (p b) (fun p (desugar b))]
    [appS (f e) (app (desugar f)
                     (desugar e))]))

(define (subst expr sub-id val)
  (type-case FAE expr
    [num (n) expr]
    [add (l r) (add (subst l sub-id val)
                    (subst r sub-id val))]
    [sub (l r) (sub (subst l sub-id val)
                    (subst r sub-id val))]
    [id (v) (if (symbol=? v sub-id) val expr)]
    [fun (p b) (if (symbol=? p sub-id)
                   expr
                   (fun p (subst b sub-id val)))]
    [app (f e) (app (subst f sub-id val)
                    (subst e sub-id val))]))


;; calc : FAE -> number
(define (calc expr)
  (type-case FAE expr
    [num (n) n]
    [add (l r) (+ (calc l) (calc r))]
    [sub (l r) (- (calc l) (calc r))]
    [id (v) (error 'calc "free identifier")]
    [else empty]))

;;(test (calc (parse '3)) 3)
;;(test (calc (parse '{+ 3 4})) 7)
;;(test (calc (parse '{+ {- 3 4} 7})) 6)
;;(test (calc (parse '{with {x {+ 5 5}} {+ x x}})) 20)
;;(test (calc (parse '{with {x 5} {+ x x}})) 10)
;;(test (calc (parse '{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}})) 14)
;;(test (calc (parse '{with {x 5} {with {y {- x 3}} {+ y y}}})) 4)
;;(test (calc (parse '{with {x 5} {+ x {with {x 3} 10}}})) 15)
;;(test (calc (parse '{with {x 5} {+ x {with {x 3} x}}})) 8)
;;(test (calc (parse '{with {x 5} {+ x {with {y 3} x}}})) 10)
;;(test (calc (parse '{with {x 5} {with {y x} y}})) 5)
;;(test (calc (parse '{with {x 5} {with {x x} x}})) 5)

;; add-numbers FAE FAE -> FAE
;; takes two num subtypes returns the sum in a new sum.
(define (add-numbers numa numb)
  (num (+ (num-n numa)
          (num-n numb))))

(define (rest-numbers numa numb)
  (num (- (num-n numa)
          (num-n numb))))

;; interp : FAE -> FAE
;; evaluates FAE expressions by reducing them to their corresponding values
;; return values are either num or fun
(define (interp expr)
  (type-case FAE expr
    [num (n) expr]
    [add (l r) (add-numbers (interp l) (interp r))]
    [sub (l r) (rest-numbers (interp l) (interp r))]
    [id (v) (error 'interp "free identifier")]
    [fun (bound-id bound-body)
         expr]
    [app (fun-expr arg-expr)
         (local ([define fun-val (interp fun-expr)])
           (interp (subst (fun-body fun-val)
                          (fun-param fun-val)
                          (interp arg-expr))))]))
(test (interp (desugar (parse '3))) (num 3))
(test (interp (desugar (parse '{+ 3 4}))) (num 7))
(test (interp (desugar (parse '{+ {- 3 4} 7}))) (num 6))
(test (interp (desugar (parse '{with {x {+ 5 5}} {+ x x}}))) (num 20))
(test (interp (desugar (parse '{with {x 5} {+ x x}}))) (num 10))
(test (interp (desugar (parse '{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}}))) (num 14))
(test (interp (desugar (parse '{with {x 5} {with {y {- x 3}} {+ y y}}}))) (num 4))
(test (interp (desugar (parse '{with {x 5} {+ x {with {x 3} 10}}}))) (num 15))
(test (interp (desugar (parse '{with {x 5} {+ x {with {x 3} x}}}))) (num 8))
(test (interp (desugar (parse '{with {x 5} {+ x {with {y 3} x}}}))) (num 10))
(test (interp (desugar (parse '{with {x 5} {with {y x} y}}))) (num 5))
(test (interp (desugar (parse '{with {x 5} {with {x x} x}}))) (num 5))
(test (interp (desugar (parse '{{fun {x} x} 3}))) (num 3))
(test (interp (desugar (parse '{{{fun {x} x} {fun {x} {+ x 5}}} 3}))) (num 8))
(test (interp (desugar (parse '{with {x 3} {fun {y} {+ x y}}}))) (fun 'y (add (num 3) (id 'y))))
(test (interp (desugar (parse '{with {x 10} {{fun {y} {+ y x}} {+ 5 x}}}))) (num 25))
(test (interp (desugar (parse '{with {x 1}
                                     {with {x {+ 1 1}}
                                           {with {foo {fun {y} {+ y {+ x {+ x y}}}}}
                                                 {with {x {+ 1 2}}
                                                       {with {x {+ 2 2}}
                                                             {foo 5}}}}}}))) (num 14))