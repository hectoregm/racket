#lang plai

(print-only-errors true)

(define-type CFAES/L
  [numS (n number?)]
  [addS (lhs CFAES/L?)
        (rhs CFAES/L?)]
  [subS (lhs CFAES/L?)
        (rhs CFAES/L?)]
  [withS (name symbol?)
         (named-expr CFAES/L?)
         (body CFAES/L?)]
  [idS (name symbol?)]
  [funS (param symbol?)
        (body CFAES/L?)]
  [appS (fun-expr CFAES/L?)
        (arg-expr CFAES/L?)])

(define-type CFAE/L
  [num (n number?)]
  [add (lhs CFAE/L?)
       (rhs CFAE/L?)]
  [sub (lhs CFAE/L?)
       (rhs CFAE/L?)]
  [id (name symbol?)]
  [fun (param symbol?)
       (body CFAE/L?)]
  [app (fun-expr CFAE/L?)
       (arg-expr CFAE/L?)])

(define-type Env
  [mtSub]
  [aSub (name symbol?)
        (value CFAE/L-Value?)
        (env Env?)])

(define-type CFAE/L-Value
  [numV (n number?)]
  [closureV (param symbol?)
            (body CFAE/L?)
            (env Env?)]
  [exprV (expr CFAE/L?)
         (env Env?)])

;; lookup : symbol Env -> FAE-Value
(define (lookup name env)
  (type-case Env env
    [mtSub () (error 'lookup "no binding for identifier")]
    [aSub (bound-name bound-value env)
          (if (symbol=? bound-name name)
              bound-value
              (lookup name env))]))

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
  (type-case CFAES/L expr
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

(define (cparser sexp)
  (desugar (parse sexp)))
(test (cparser '{+ 1 2}) (add (num 1) (num 2)))
(test (cparser '{+ {- 1 2} 3}) (add (sub (num 1) (num 2)) (num 3)))
(test (cparser '{with {x 3} x}) (app (fun 'x (id 'x)) (num 3)))
(test (cparser '{with {x 10} {with {y {+ x 5}} {+ x y}}}) (app (fun 'x (app (fun 'y (add (id 'x) (id 'y))) (add (id 'x) (num 5)))) (num 10)))

;; num+ CFAE/L-Value CFAE/L-Value -> CFAE/L-Value
(define (num+ numa numb)
  (numV (+ (numV-n (strict numa))
           (numV-n (strict numb)))))

(define (num- numa numb)
  (numV (- (numV-n (strict numa))
           (numV-n (strict numb)))))

;; strict : CFAE/L-Value -> CFAE/L-Value [excluding exprV]
(define (strict e)
  (type-case CFAE/L-Value e
    [exprV (expr env) (strict (interp expr env))]
    [else e]))

;; interp : CFAE/L Env -> CFAE/L-Value
(define (interp expr env)
  (type-case CFAE/L expr
    [num (n) (numV n)]
    [add (l r) (num+ (interp l env) (interp r env))]
    [sub (l r) (num- (interp l env) (interp r env))]
    [id (v) (lookup v env)]
    [fun (bound-id bound-body)
         (closureV bound-id bound-body env)]
    [app (fun-expr arg-expr)
         (local ([define fun-val (strict (interp fun-expr env))]
                 [define arg-val (exprV arg-expr env)])
           (if (closureV? fun-val)
               (interp (closureV-body fun-val)
                       (aSub (closureV-param fun-val)
                             arg-val
                             (closureV-env fun-val)))
               (error 'interp (string-append (~a fun-expr) " expression is not a function"))))]))
(test (interp (desugar (parse '3)) (mtSub)) (numV 3))
(test (interp (desugar (parse '{fun {x} x})) (mtSub)) (closureV 'x (id 'x) (mtSub)))
(test (interp (desugar (parse '{+ 3 4})) (mtSub)) (numV 7))
(test (interp (desugar (parse '{+ {- 3 4} 7})) (mtSub)) (numV 6))
(test (interp (desugar (parse '{with {x {+ 5 5}} {+ x x}})) (mtSub)) (numV 20))
(test (interp (desugar (parse '{with {x 5} {+ x x}})) (mtSub)) (numV 10))
(test (interp (desugar (parse '{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}})) (mtSub)) (numV 14))
(test (interp (desugar (parse '{with {x 5} {with {y {- x 3}} {+ y y}}})) (mtSub)) (numV 4))
(test (interp (desugar (parse '{with {x 5} {+ x {with {x 3} 10}}})) (mtSub)) (numV 15))
(test (interp (desugar (parse '{with {x 5} {+ x {with {x 3} x}}})) (mtSub)) (numV 8))
(test (interp (desugar (parse '{with {x 5} {+ x {with {y 3} x}}})) (mtSub)) (numV 10))
(test (strict (interp (desugar (parse '{with {x 5} {with {y x} y}})) (mtSub))) (numV 5))
(test (strict (interp (desugar (parse '{with {x 5} {with {x x} x}})) (mtSub))) (numV 5))
(test (strict (interp (desugar (parse '{{fun {x} x} 3})) (mtSub))) (numV 3))
(test (interp (desugar (parse '{{{fun {x} x} {fun {x} {+ x 5}}} 3})) (mtSub)) (numV 8))
(test (interp (desugar (parse '{with {x 3} {fun {y} {+ x y}}})) (mtSub)) (closureV 'y (add (id 'x) (id 'y)) (aSub 'x (exprV (num 3) (mtSub)) (mtSub))))
(test (interp (desugar (parse '{with {x 3} {with {f {fun {y} {+ x y}}} {with {x 5} {f 4}}}})) (mtSub)) (numV 7))
(test (interp (desugar (parse '{with {x 10} {{fun {y} {+ y x}} {+ 5 x}}})) (mtSub)) (numV 25))
(test (interp (cparser '{with {double {fun {x} {+ x x}}} {+ {double 5} {double 10}}}) (mtSub)) (numV 30))
