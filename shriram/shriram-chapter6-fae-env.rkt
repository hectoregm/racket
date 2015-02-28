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

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?)
        (value FAE-Value?)
        (ds DefrdSub?)])

(define-type FAE-Value
  [numV (n number?)]
  [closureV (param symbol?)
            (body FAE?)
            (ds DefrdSub?)])

;; lookup : symbol DefrdSub -> FAE-Value
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "no binding for identifier")]
    [aSub (bound-name bound-value rest-ds)
          (if (symbol=? bound-name name)
              bound-value
              (lookup name rest-ds))]))

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

;; add-numbers FAE FAE -> FAE
;; takes two num subtypes returns the sum in a new sum.
(define (num+ numa numb)
  (numV (+ (numV-n numa)
           (numV-n numb))))

(define (num- numa numb)
  (numV (- (numV-n numa)
           (numV-n numb))))

;; interp : FAE -> FAE
;; evaluates FAE expressions by reducing them to their corresponding values
;; return values are either num or fun
(define (interp expr ds)
  (type-case FAE expr
    [num (n) (numV n)]
    [add (l r) (num+ (interp l ds) (interp r ds))]
    [sub (l r) (num- (interp l ds) (interp r ds))]
    [id (v) (lookup v ds)]
    [fun (bound-id bound-body)
         (closureV bound-id bound-body ds)]
    [app (fun-expr arg-expr)
         (local ([define fun-val (interp fun-expr ds)])
           (interp (closureV-body fun-val)
                   (aSub (closureV-param fun-val)
                         (interp arg-expr ds)
                         (closureV-ds fun-val))))]))
(test (interp (desugar (parse '3)) (mtSub)) (numV 3))
(test (interp (desugar (parse '{+ 3 4})) (mtSub)) (numV 7))
(test (interp (desugar (parse '{+ {- 3 4} 7})) (mtSub)) (numV 6))
(test (interp (desugar (parse '{with {x {+ 5 5}} {+ x x}})) (mtSub)) (numV 20))
(test (interp (desugar (parse '{with {x 5} {+ x x}})) (mtSub)) (numV 10))
(test (interp (desugar (parse '{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}})) (mtSub)) (numV 14))
(test (interp (desugar (parse '{with {x 5} {with {y {- x 3}} {+ y y}}})) (mtSub)) (numV 4))
(test (interp (desugar (parse '{with {x 5} {+ x {with {x 3} 10}}})) (mtSub)) (numV 15))
(test (interp (desugar (parse '{with {x 5} {+ x {with {x 3} x}}})) (mtSub)) (numV 8))
(test (interp (desugar (parse '{with {x 5} {+ x {with {y 3} x}}})) (mtSub)) (numV 10))
(test (interp (desugar (parse '{with {x 5} {with {y x} y}})) (mtSub)) (numV 5))
(test (interp (desugar (parse '{with {x 5} {with {x x} x}})) (mtSub)) (numV 5))
(test (interp (desugar (parse '{{fun {x} x} 3})) (mtSub)) (numV 3))
(test (interp (desugar (parse '{{{fun {x} x} {fun {x} {+ x 5}}} 3})) (mtSub)) (numV 8))
(test (interp (desugar (parse '{with {x 3} {fun {y} {+ x y}}})) (mtSub)) (closureV 'y (add (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub))))
(test (interp (desugar (parse '{with {x 3} {with {f {fun {y} {+ x y}}} {with {x 5} {f 4}}}})) (mtSub)) (numV 7))
(test (interp (desugar (parse '{with {x 10} {{fun {y} {+ y x}} {+ 5 x}}})) (mtSub)) (numV 25))
