#lang plai

(print-only-errors true)

(define-type CFAES/L
  [numS (n number?)]
  [addS (lhs CFAES/L?)
        (rhs CFAES/L?)]
  [multS (lhs CFAES/L?)
         (rhs CFAES/L?)]
  [divS (lhs CFAES/L?)
        (rhs CFAES/L?)]
  [subS (lhs CFAES/L?)
        (rhs CFAES/L?)]
  [withS (name symbol?)
         (named-expr CFAES/L?)
         (body CFAES/L?)]
  [idS (name symbol?)]
  [if0S (test CFAES/L?)
        (truth CFAES/L?)
        (falsity CFAES/L?)]
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
  [mult (lhs CFAE/L?)
        (rhs CFAE/L?)]
  [div (lhs CFAE/L?)
       (rhs CFAE/L?)]
  [id (name symbol?)]
  [if0 (test CFAE/L?)
       (truth CFAE/L?)
       (falsity CFAE/L?)]
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
    [mtSub () (error 'lookup (string-append (~a name) " expression is not a function"))]
    [aSub (bound-name bound-value env)
          (if (symbol=? bound-name name)
              bound-value
              (lookup name env))]))

;; num-zero? CFAE/L-Value -? boolean
(define (num-zero? n)
  (zero? (numV-n (strict n))))

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
       [(*) (multS (parse (second sexp))
                   (parse (third sexp)))]
       [(/) (divS (parse (second sexp))
                  (parse (third sexp)))]
       [(if0) (if0S (parse (cadr sexp))
                    (parse (caddr sexp))
                    (parse (cadddr sexp)))]
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
    [multS (l r) (mult (desugar l)
                       (desugar r))]
    [divS (l r) (div (desugar l)
                     (desugar r))]
    [if0S (t p f) (if0 (desugar t)
                       (desugar p)
                       (desugar f))]
    [withS (id named body) (app (fun id (desugar body))
                                (desugar named))]
    [idS (s) (id s)]
    [funS (p b) (fun p (desugar b))]
    [appS (f e) (app (desugar f)
                     (desugar e))]))

(define (cparse sexp)
  (desugar (parse sexp)))
(test (cparse '{+ 1 2}) (add (num 1) (num 2)))
(test (cparse '{+ {- 1 2} 3}) (add (sub (num 1) (num 2)) (num 3)))
(test (cparse '{with {x 3} x}) (app (fun 'x (id 'x)) (num 3)))
(test (cparse '{with {x 10} {with {y {+ x 5}} {+ x y}}}) (app (fun 'x (app (fun 'y (add (id 'x) (id 'y))) (add (id 'x) (num 5)))) (num 10)))

;; num+ CFAE/L-Value CFAE/L-Value -> CFAE/L-Value
(define (num+ numa numb)
  (numV (+ (numV-n (strict numa))
           (numV-n (strict numb)))))

(define (num- numa numb)
  (numV (- (numV-n (strict numa))
           (numV-n (strict numb)))))

(define (num* numa numb)
  (numV (* (numV-n (strict numa))
           (numV-n (strict numb)))))

(define (num/ numa numb)
  (numV (/ (numV-n (strict numa))
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
    [mult (l r) (num* (interp l env) (interp r env))]
    [div (l r) (num/ (interp l env) (interp r env))]
    [id (v) (lookup v env)]
    [if0 (test truth falsity)
         (if (num-zero? (interp test env))
             (interp truth env)
             (interp falsity env))]
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

(define (rinterp expr)
  (strict (interp expr (mtSub))))

(test (rinterp (cparse '3)) (numV 3))
(test (rinterp (cparse '{fun {x} x})) (closureV 'x (id 'x) (mtSub)))
(test (rinterp (cparse '{+ 3 4})) (numV 7))
(test (rinterp (cparse '{+ {- 3 4} 7})) (numV 6))
(test (rinterp (cparse '{with {x {+ 5 5}} {+ x x}})) (numV 20))
(test (rinterp (cparse '{with {x 5} {+ x x}})) (numV 10))
(test (rinterp (cparse '{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}})) (numV 14))
(test (rinterp (cparse '{with {x 5} {with {y {- x 3}} {+ y y}}})) (numV 4))
(test (rinterp (cparse '{with {x 5} {+ x {with {x 3} 10}}})) (numV 15))
(test (rinterp (cparse '{with {x 5} {+ x {with {x 3} x}}})) (numV 8))
(test (rinterp (cparse '{with {x 5} {+ x {with {y 3} x}}})) (numV 10))
(test (rinterp (cparse '{with {x 5} {with {y x} y}})) (numV 5))
(test (rinterp (cparse '{with {x 5} {with {x x} x}})) (numV 5))
(test (rinterp (cparse '{{fun {x} x} 3})) (numV 3))
(test (rinterp (cparse '{{{fun {x} x} {fun {x} {+ x 5}}} 3})) (numV 8))
(test (rinterp (cparse '{with {x 3} {fun {y} {+ x y}}})) (closureV 'y (add (id 'x) (id 'y)) (aSub 'x (exprV (num 3) (mtSub)) (mtSub))))
(test (rinterp (cparse '{with {x 3} {with {f {fun {y} {+ x y}}} {with {x 5} {f 4}}}})) (numV 7))
(test (rinterp (cparse '{with {x 10} {{fun {y} {+ y x}} {+ 5 x}}})) (numV 25))
(test (rinterp (cparse '{with {double {fun {x} {+ x x}}} {+ {double 5} {double 10}}})) (numV 30))
(test (rinterp (cparse '{with {f {undef x}} 4})) (numV 4))
(test (rinterp (cparse '{if0 {+ 0 0} 0 1})) (numV 0))
(test (rinterp (cparse '{with {f {fun {x} {+ x x}}} {if0 {- 5 {+ 2 3}} {f 2} {f 3}}})) (numV 4))
(test (rinterp (cparse '{with {f {fun {x} {+ x x}}} {if0 {- 5 {+ 2 4}} {f 2} {f 3}}})) (numV 6))

(test (rinterp (cparse '{with {Y {fun {le} {{fun {f} {f f}} {fun {f} {le {fun {x} {{f f} x}}}}}}} {{Y {fun {factorial} {fun {n} {if0 n 1 {* n {factorial {- n 1}}}}}}} 6}})) (numV 720))

(test (rinterp (cparse '{with {x 1}
                              {with {x {+ 1 1}}
                                    {with {foo {fun {y} {+ y {+ x {+ x y}}}}}
                                          {with {x {+ 1 2}}
                                                {with {x {+ 2 2}}
                                                      {foo 5}}}}}})) (numV 14))