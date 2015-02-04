#lang plai

(print-only-errors true)

(define-type F1WAE
  [num (n number?)]
  [add (lhs F1WAE?)
       (rhs F1WAE?)]
  [sub (lhs F1WAE?)
       (rhs F1WAE?)]
  [with (name symbol?)
        (named-expr F1WAE?)
        (body F1WAE?)]
  [id (name symbol?)]
  [app (fun-name symbol?) (arg F1WAE?)])

(define-type FunDef
  [fundef (fun-name symbol?)
          (arg-name symbol?)
          (body F1WAE?)])

;; lookup-fundef : symbol listof(FunDef) -→ FunDef 
(define (lookup-fundef fun-name fundefs)
  (cond
    [(empty? fundefs) (error fun-name "function not found")]
    [else (if (symbol=? fun-name (fundef-fun-name (first fundefs)))
              (first fundefs)
              (lookup-fundef fun-name (rest fundefs)))]))

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
       [(with) (with (first (second sexp))
                     (parse (second (second sexp)))
                     (parse (third sexp)))]
       [else (app (first sexp)
                  (parse (second sexp)))])]))

(define (subst expr sub-id val)
  (type-case F1WAE expr
    [num (n) expr]
    [add (l r) (add (subst l sub-id val)
                    (subst r sub-id val))]
    [sub (l r) (sub (subst l sub-id val)
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
    [app (fun-name arg-expr)
         (app fun-name (subst arg-expr sub-id val))]))


;; calc : F1WAE -> number
(define (calc expr)
  (type-case F1WAE expr
    [num (n) n]
    [add (l r) (+ (calc l) (calc r))]
    [sub (l r) (- (calc l) (calc r))]
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

;; interp : F1WAE listof(fundef) -> number
;; evaluates F1WAE expressions by reducing them to their corresponding values

(define (interp expr fun-defs)
  (type-case F1WAE expr
  [num (n) n]
  [add (l r) (+ (interp l fun-defs)
                (interp r fun-defs))]
  [sub (l r) (- (interp l fun-defs)
                (interp r fun-defs))]
  [with (bound-id named-expr bound-body)
        (interp (subst bound-body
                       bound-id
                       (num (interp named-expr fun-defs))) 
                fun-defs)]
  [id (v) (error 'interp "free identifier")]
  [app (fun-name arg-expr)
       (local ([define the-fun-def (lookup-fundef fun-name fun-defs)])
         (interp (subst (fundef-body the-fun-def)
                        (fundef-arg-name the-fun-def)
                        (num (interp arg-expr fun-defs)))
                 fun-defs))]))

(test (interp (parse '3) '()) 3)
(test (interp (parse '{+ 3 4}) '()) 7)
(test (interp (parse '{+ {- 3 4} 7}) '()) 6)
(test (interp (parse '{with {x {+ 5 5}} {+ x x}}) '()) 20)
(test (interp (parse '{with {x 5} {+ x x}}) '()) 10)
(test (interp (parse '{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}}) '()) 14)
(test (interp (parse '{with {x 5} {with {y {- x 3}} {+ y y}}}) '()) 4)
(test (interp (parse '{with {x 5} {+ x {with {x 3} 10}}}) '()) 15)
(test (interp (parse '{with {x 5} {+ x {with {x 3} x}}}) '()) 8)
(test (interp (parse '{with {x 5} {+ x {with {y 3} x}}}) '()) 10)
(test (interp (parse '{with {x 5} {with {y x} y}}) '()) 5)
(test (interp (parse '{with {x 5} {with {x x} x}}) '()) 5)

(test (interp (parse '{double {double 5}})
          (list (fundef 'double
                        'n
                        (add (id 'n) (id 'n))))) 20)
(test (interp (parse '{f 5})
              (list (fundef 'f 'n (app 'g (add (id 'n) (num 5))))
                    (fundef 'g 'm (sub (id 'm) (num 1))))) 9)