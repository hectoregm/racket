#lang plai

(print-only-errors true)

(define-type BCFAES
  [numS (n number?)]
  [withS (name symbol?)
         (named-expr BCFAES?)
         (body BCFAES?)]
  [idS (name symbol?)]
  [if0S (test BCFAES?)
        (truth BCFAES?)
        (falsity BCFAES?)]
  [funS (param symbol?)
        (body BCFAES?)]
  [appS (fun-expr BCFAES?)
        (arg-expr BCFAES?)]
  [binopS (f procedure?)
          (l BCFAES?)
          (r BCFAES?)]
  [newboxS (value-expr BCFAES?)]
  [setboxS (box-expr BCFAES?)
           (value-epxr BCFAES?)]
  [openboxS (box-expr BCFAES?)]
  [seqnS (expr1 BCFAES?)
         (expr2 BCFAES?)])

(define-type BCFAE
  [num (n number?)]
  [id (name symbol?)]
  [if0 (test BCFAE?)
        (truth BCFAE?)
        (falsity BCFAE?)]
  [fun (param symbol?)
       (body BCFAE?)]
  [app (fun-expr BCFAE?)
       (arg-expr BCFAE?)]
  [binop (f procedure?)
         (l BCFAE?)
         (r BCFAE?)]
  [newbox (value-expr BCFAE?)]
  [setbox (box-expr BCFAE?)
          (value-epxr BCFAE?)]
  [openbox (box-expr BCFAE?)]
  [seqn (expr1 BCFAE?)
        (expr2 BCFAE?)])

(define-type BCFAE-Value
  [numV (n number?)]
  [closureV (param symbol?)
            (body BCFAE?)
            (env Env?)]
  [boxV (location number?)])

(define-type Env
  [mtSub]
  [aSub (name symbol?) 
        (location number?)
        (env Env?)])

(define-type Store
  [mtSto]
  [aSto (location number?)
        (value BCFAE-Value?)
        (store Store?)])

(define-type Value*Store
  [v*s (value BCFAE-Value?)
       (store Store?)])

(define (lookup-op s)
  (case s
    [(+) +]
    [(-) -]
    [(*) *]
    [(/) /]))

;; num-zero? BCFAE-Value -> boolean
(define (num-zero? n)
  (zero? (numV-n n)))

;; A::= <number>|<symbol>|listof(<A>)
;; parse: A -> BCFAES
(define (parse sexp)
  (cond
    [(number? sexp) (numS sexp)]
    [(symbol? sexp) (idS sexp)]
    [(list? sexp)
     (case (first sexp)
       [(with) (withS (first (second sexp))
                     (parse (second (second sexp)))
                     (parse (third sexp)))]
       [(fun) (funS (first (second sexp))
                   (parse (third sexp)))]
       [(+ - / *) (binopS (lookup-op (car sexp)) (parse (cadr sexp)) (parse (caddr sexp)))]
       [(if0) (if0S (parse (cadr sexp))
                    (parse (caddr sexp))
                    (parse (cadddr sexp)))]
       [(newbox) (newboxS (parse (cadr sexp)))]
       [(setbox) (setboxS (parse (cadr sexp)) (parse (caddr sexp)))]
       [(openbox) (openboxS (parse (cadr sexp)))]
       [(seqn) (seqnS (parse (cadr sexp)) (parse (caddr sexp)))]
       [else (appS (parse (first sexp))
                   (parse (second sexp)))])]))

;; env-lookup : symbol Env -> location
(define (env-lookup name env)
  (type-case Env env
    [mtSub () (error 'env-lookup (string-append (~a name) " binding is not in the env"))]
    [aSub (bound-name bound-location env)
          (if (symbol=? bound-name name)
              bound-location
              (env-lookup name env))]))

;; store-lookup : symbol Store -> BCFAE-Value
(define (store-lookup loc-index store)
  (type-case Store store
    [mtSto () (error 'store-lookup (string-append (~a loc-index) " no value at location"))]
    [aSto (location value store)
          (if (= location loc-index)
              value
              (store-lookup loc-index store))]))

(define (desugar expr)
  (type-case BCFAES expr
    [numS (n) (num n)]
    [idS (s) (id s)]
    [binopS (f l r) (binop f (desugar l) (desugar r))]
    [withS (id named body) (app (fun id (desugar body))
                                (desugar named))]
    [if0S (t p f) (if0 (desugar t)
                       (desugar p)
                       (desugar f))]
    [funS (p b) (fun p (desugar b))]
    [appS (f e) (app (desugar f)
                     (desugar e))]
    [newboxS (value-expr) (newbox (desugar value-expr))]
    [setboxS (box-expr value-expr) (setbox (desugar box-expr) (desugar value-expr))]
    [openboxS (box-expr) (openbox (desugar box-expr))]
    [seqnS (expr1 expr2) (seqn (desugar expr1) (desugar expr2))]))

(define (cparse sexp)
  (desugar (parse sexp)))

(define (apply-binop fun num-a num-b)
  (numV (fun (numV-n num-a)
             (numV-n num-b))))

(define next-location
  (local ([define last-loc (box -1)])
    (lambda (store)
      (begin
        (set-box! last-loc (+ 1 (unbox last-loc)))
        (unbox last-loc)))))

;; interp BCFAE Env Store -> ValuexStore
(define (interp expr env store)
  (type-case BCFAE expr
    [num (n) (v*s (numV n) store)]
    [id (v) (begin
              ;;(display env)
              (v*s (store-lookup (env-lookup v env) store) store))]
    [if0 (test truth falsity)
         (type-case Value*Store (interp test env store)
           [v*s (test-value test-store)
                (if (num-zero? test-value)
                    (interp truth env test-store)
                    (interp falsity env test-store))])]
    [fun (param bound-body)
         (v*s (closureV param bound-body env) store)]
    [app (fun-expr arg-expr)
         (type-case Value*Store (interp fun-expr env store)
           [v*s (fun-value fun-store)
                (type-case Value*Store (interp arg-expr env fun-store)
                  [v*s (arg-value arg-store)
                       (local ([define new-loc (next-location arg-store)])
                         (interp (closureV-body fun-value)
                                 (aSub (closureV-param fun-value)
                                       new-loc
                                       (closureV-env fun-value))
                                 (aSto new-loc
                                       arg-value
                                       arg-store)))])])]
;         (local ([define fun-val (interp fun-expr env)])
;           (if (closureV? fun-val)
;               (interp (closureV-body fun-val)
;                       (aSub (closureV-param fun-val)
;                             (interp arg-expr env)
;                             (closureV-env fun-val)))
;               (error 'interp (string-append (~a fun-expr) " expression is not a function"))))]
        [binop (f l r)
           (type-case Value*Store (interp l env store)
             [v*s (l-value l-store)
                  (type-case Value*Store (interp r env l-store)
                    [v*s (r-value r-store)
                         (v*s (apply-binop f l-value r-value)
                              r-store)])])]
    [newbox (value-expr)
            (type-case Value*Store (interp value-expr env store)
              [v*s (expr-value expr-store)
                   (local ([define new-loc (next-location expr-store)])
                     (v*s (boxV new-loc)
                          (aSto new-loc expr-value expr-store)))])]
    [setbox (box-expr value-expr)
            (type-case Value*Store (interp box-expr env store)
              [v*s (box-value box-store)
                   (type-case Value*Store (interp value-expr env box-store)
                     [v*s (value-value value-store)
                          (v*s value-value
                               (aSto (boxV-location box-value)
                                     value-value
                                     value-store))])])]
    [openbox (box-expr)
             (type-case Value*Store (interp box-expr env store)
               [v*s (box-value box-store)
                    (v*s (store-lookup (boxV-location box-value)
                                       box-store)
                         box-store)])]
    [seqn (e1 e2)
          (type-case Value*Store (interp e1 env store)
            [v*s (e1-value e1-store)
                 (interp e2 env e1-store)])]))

(define (rinterp expr)
  (interp expr (mtSub) (mtSto)))

(test (rinterp (cparse '3)) (v*s (numV 3) (mtSto)))
(test (rinterp (cparse '{+ 3 4})) (v*s (numV 7) (mtSto)))
(test (rinterp (cparse '{+ {- 3 4} 7})) (v*s (numV 6) (mtSto)))
(test (rinterp (cparse '{with {x {+ 5 5}} {+ x x}})) (v*s (numV 20) (aSto 0 (numV 10) (mtSto))))
(test (rinterp (cparse '{with {x 5} {+ x x}})) (v*s (numV 10) (aSto 1 (numV 5) (mtSto))))
(test (rinterp (cparse '{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}})) (v*s (numV 14) (aSto 3 (numV 7) (aSto 2 (numV 10) (mtSto)))))
(test (rinterp (cparse '{with {x 5} {+ x {with {x 3} 10}}})) (v*s (numV 15) (aSto 5 (numV 3) (aSto 4 (numV 5) (mtSto)))))
(test (rinterp (cparse '{with {x 5} {+ x {with {x 3} x}}}))  (v*s (numV 8) (aSto 7 (numV 3) (aSto 6 (numV 5) (mtSto)))))
(test (rinterp (cparse '{with {x 5} {+ x {with {y 3} x}}})) (v*s (numV 10) (aSto 9 (numV 3) (aSto 8 (numV 5) (mtSto)))))
(test (rinterp (cparse '{with {x 5} {with {y x} y}})) (v*s (numV 5) (aSto 11 (numV 5) (aSto 10 (numV 5) (mtSto)))))
(test (rinterp (cparse '{with {x 5} {with {x x} x}})) (v*s (numV 5) (aSto 13 (numV 5) (aSto 12 (numV 5) (mtSto)))))
(test (rinterp (cparse '{{fun {x} x} 3})) (v*s (numV 3) (aSto 14 (numV 3) (mtSto))))
(test (rinterp (cparse '{{{fun {x} x} {fun {x} {+ x 5}}} 3})) (v*s (numV 8)
                                                                   (aSto
                                                                    16
                                                                    (numV 3)
                                                                    (aSto 15 (closureV 'x (binop + (id 'x) (num 5)) (mtSub)) (mtSto)))))
(test (rinterp (cparse '{with {x 3} {fun {y} {+ x y}}})) (v*s
                                                          (closureV 'y (binop + (id 'x) (id 'y)) (aSub 'x 17 (mtSub)))
                                                          (aSto 17 (numV 3) (mtSto))))
(test (rinterp (cparse '{with {x 10} {{fun {y} {+ y x}} {+ 5 x}}})) (v*s (numV 25) (aSto 19 (numV 15) (aSto 18 (numV 10) (mtSto)))))
(test (rinterp (cparse '{if0 {+ 0 0} 0 1})) (v*s (numV 0) (mtSto)))
(test (rinterp (cparse '{if0 {+ 0 -5} 1 1})) (v*s (numV 1) (mtSto)))
(test (v*s-value (rinterp (cparse '{with {Y {fun {le} {{fun {f} {f f}} {fun {f} {le {fun {x} {{f f} x}}}}}}} {{Y {fun {factorial} {fun {n} {if0 n 1 {* n {factorial {- n 1}}}}}}} 6}}))) (numV 720))

(test (v*s-value (rinterp (cparse '{with {b {newbox 0}}
                                         {if0 {seqn {setbox b 5}
                                                    {openbox b}}
                                              1
                                              {openbox b}}}))) (numV 5))
(test (v*s-value (rinterp (cparse '{with {b {newbox 0}}
                                         {if0 {seqn {setbox b 5}
                                                    {openbox b}}
                                              1
                                              b}}))) (boxV 52))