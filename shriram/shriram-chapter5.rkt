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

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?)
        (value number?)
        (ds DefrdSub?)])

(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "no binding for identifier")]
    [aSub (bound-name bound-value rest-ds)
          (if (symbol=? bound-name name)
              bound-value
              (lookup name rest-ds))]))

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

;; interp : F1WAE listof(fundef) DefrdSub -> number
;; evaluates F1WAE expressions by reducing them to their corresponding values
(define (interp expr fun-defs ds)
  (type-case F1WAE expr
    [num (n) n]
    [add (l r) (+ (interp l fun-defs ds)
                  (interp r fun-defs ds))]
    [sub (l r) (- (interp l fun-defs ds)
                  (interp r fun-defs ds))]
    [with (bound-id named-expr bound-body)
          (interp bound-body
                  fun-defs
                  (aSub bound-id
                        (interp named-expr
                                fun-defs
                                ds)
                        ds))]
    [id (v) (lookup v ds)]
    [app (fun-name arg-expr)
         (local ([define the-fun-def (lookup-fundef fun-name fun-defs)])
           (interp (fundef-body the-fun-def)
                   fun-defs
                   (aSub (fundef-arg-name the-fun-def)
                         (interp arg-expr fun-defs ds)
                         (mtSub))))]))

(test (interp (parse '3) '() (mtSub)) 3)
(test (interp (parse '{+ 3 4}) '() (mtSub)) 7)
(test (interp (parse '{+ {- 3 4} 7}) '() (mtSub)) 6)
(test (interp (parse '{with {x {+ 5 5}} {+ x x}}) '() (mtSub)) 20)
(test (interp (parse '{with {x 5} {+ x x}}) '() (mtSub)) 10)
(test (interp (parse '{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}}) '() (mtSub)) 14)
(test (interp (parse '{with {x 5} {with {y {- x 3}} {+ y y}}}) '() (mtSub)) 4)
(test (interp (parse '{with {x 5} {+ x {with {x 3} 10}}}) '() (mtSub)) 15)
(test (interp (parse '{with {x 5} {+ x {with {x 3} x}}}) '() (mtSub)) 8)
(test (interp (parse '{with {x 5} {+ x {with {y 3} x}}}) '() (mtSub)) 10)
(test (interp (parse '{with {x 5} {with {y x} y}}) '() (mtSub)) 5)
(test (interp (parse '{with {x 5} {with {x x} x}}) '() (mtSub)) 5)

(test (interp (parse '{double {double 5}})
              (list (fundef 'double
                            'n
                            (add (id 'n) (id 'n)))) (mtSub)) 20)

(test (interp (parse '{f 5})
              (list (fundef 'f 'n (app 'g (add (id 'n) (num 5))))
                    (fundef 'g 'm (sub (id 'm) (num 1)))) (mtSub)) 9)

(test/exn (interp (parse '{with {n 5} {f 10}}) (list (fundef 'f 'p (id 'n))) (mtSub)) "no binding")
