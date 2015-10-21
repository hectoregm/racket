#lang plai

(print-only-errors false)

;; Binding - Defines a datatype that represents a given symbol
;; and its associsated CFAES expresion.
(define-type Binding
  [bind (name symbol?) (val BCFAES?)])

;; BCFAES - Datatype that defines the surface BCFAE language.
(define-type BCFAES
  [numS (n number?)]
  [withS (bindings (listof bind?))
         (body BCFAES?)]
  [idS (name symbol?)]
  [if0S (test BCFAES?)
        (truth BCFAES?)
        (falsity BCFAES?)]
  [funS (params (listof symbol?))
        (body BCFAES?)]
  [appS (fun BCFAES?)
        (args (listof BCFAES?))]
  [binopS (f procedure?)
          (l BCFAES?)
          (r BCFAES?)]
  [newboxS (value-expr BCFAES?)]
  [setboxS (box-expr BCFAES?)
           (value-epxr BCFAES?)]
  [openboxS (box-expr BCFAES?)]
  [seqnS (expr1 BCFAES?)
         (expr2 BCFAES?)])

;; BCFAE - Datatype that defines the Core BCFAE language
(define-type BCFAE
  [num (n number?)]
  [id (name symbol?)]
  [if0 (test BCFAE?)
        (truth BCFAE?)
        (falsity BCFAE?)]
  [fun (params (listof symbol?))
       (body BCFAE?)]
  [app (fun BCFAE?)
       (args (listof BCFAE?))]
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
  [closureV (param (listof symbol?))
            (body BCFAE?)
            (env Env?)])

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

;; A::= <number>|<symbol>|listof(<A>)
;; B::= (list <symbol> <A>)
;; parse-bindings: listof(B) -> listof(bind?)
(define (parse-bindings lst)
  (let ([duplicates (find-duplicate lst (lambda (e1 e2) (symbol=? (car e1) (car e2))))])
    (if (boolean? duplicates)
        (map (lambda (b) (bind (car b) (parse (cadr b)))) lst)
        (error 'parse-bindings (string-append "Id" (symbol->string (car duplicates)) " is duplicated")))))

(define (lookup-op s)
  (case s
    [(+) +]
    [(-) -]
    [(*) *]
    [(/) /]))

;; find-duplicate: listof(X) (X X -> boolean) -> X
;; Given a list, it searches for the duplicates in the list,
;; using comp. It returns the first repeated element or false.
(define (find-duplicate lst comp)
  (cond
    [(empty? lst) #f]
    [(member? (car lst) (cdr lst) comp) (car lst)]
    [else (find-duplicate (cdr lst) comp)]))

;; member?: X listof(Y) (X Y -> boolean) -> boolean
;; Determines if a given x is in the list using
;; a given comp function.
(define (member? x lst comp)
  (cond
    [(empty? lst) #f]
    [(comp (car lst) x) #t]
    [else (member? x (cdr lst) comp)]))

;; num-zero? CFAE-Value -> boolean
(define (num-zero? n)
  (zero? (numV-n n)))

;; A::= <number>|<symbol>|listof(<A>)
;; parse: A -> BCFAES
(define (parse sexp)
  (cond
    [(symbol? sexp) (idS sexp)]
    [(number? sexp) (numS sexp)]
    [(list? sexp)
     (case (car sexp)
       [(with) (withS (parse-bindings (cadr sexp)) (parse (caddr sexp)))]
       [(fun) (funS (cadr sexp) (parse (caddr sexp)))]
       [(+ - / *) (binopS (lookup-op (car sexp)) (parse (cadr sexp)) (parse (caddr sexp)))]
       [(if0) (if0S (parse (cadr sexp))
                    (parse (caddr sexp))
                    (parse (cadddr sexp)))]
       [(newbox) (newboxS (parse (cadr sexp)))]
       [(setbox) (setboxS (parse (cadr sexp)) (parse (caddr sexp)))]
       [(openbox) (openboxS (parse (cadr sexp)))]
       [(seqn) (seqnS (parse (cadr sexp)) (parse (caddr sexp)))]
       [else (appS (parse (car sexp)) (map parse (cdr sexp)))])]))

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
    [withS (bindings body) (app (fun (map (lambda (bind) 
                                            (bind-name bind)) bindings)
                                     (desugar body))
                                (map (lambda (bind)
                                       (desugar (bind-val bind))) bindings))]
    [if0S (t p f) (if0 (desugar t)
                       (desugar p)
                       (desugar f))]
    [funS (params body) (fun params (desugar body))]
    [appS (fun args) (app (desugar fun) (map (lambda (arg) (desugar arg)) args))]
    [newboxS (value-expr) (newbox (desugar value-expr))]
    [setboxS (box-expr value-expr) (setbox (desugar box-expr) (desugar value-expr))]
    [openboxS (box-expr) (openbox (desugar box-expr))]
    [seqnS (expr1 expr2) (seqn (desugar expr1) (desugar expr2))]))
(test (desugar (parse '{+ 3 4})) (binop + (num 3) (num 4)))
(test (desugar (parse '{+ {- 3 4} 7})) (binop + (binop - (num 3) (num 4)) (num 7)))
(test (desugar (parse '{with {{x {+ 5 5}}} x})) (app (fun '(x) (id 'x)) (list (binop + (num 5) (num 5))) ))

(define (cparse sexp)
  (desugar (parse sexp)))

(define (apply-binop fun num-a num-b)
  (numV (fun (numV-n num-a)
             (numV-n num-b))))

(define (extend-env params args env)
  (cond
    [(empty? params) env]
    [else (aSub (car params)
                (car args)
                (extend-env (cdr params) (cdr args) env))]))

(define (interp-args args env store)
  (cond
    [(empty? args) ]))

;; interp BCFAE Env Store -> ValuexStore
(define (interp expr env store)
  (type-case BCFAE expr
    [num (n) (v*s (numV n) store)]
    [id (v) (v*s (store-lookup (env-lookup v env) store) store)]
    [if0 (test truth falsity)
         (type-case Value*Store (interp test env store)
           [v*s (test-value test-store)
                (if (num-zero? test-value)
                    (interp truth env test-store)
                    (interp falsity env test-store))])]
    [fun (params bound-body)
         (v*s (closureV params bound-body env) store)]
    [app (fun-expr args)
         (type-case Value*Store (interp fun-expr env store)
           [v*s (fun-value fun-store)
                (type-case Value*Store )])]
;         (local ([define fun-val (interp fun-expr env store)])
;           (interp (closureV-body fun-val)
;                   (extend-env (closureV-param fun-val)
;                               (map (lambda (arg) (interp arg env store)) args)
;                               (closureV-env fun-val))
;                   store))]
    [binop (f l r)
           (type-case Value*Store (interp l env store)
             [v*s (l-value l-store)
                  (type-case Value*Store (interp r env l-store)
                    [v*s (r-value r-store)
                         (v*s (apply-binop f l-value r-value)
                              r-store)])])]
    [newbox (value-expr) (error 'interp "newbox not implemented")]
    [setbox (box-expr value-expr) (error 'interp "setbox not implemented")]
    [openbox (box-expr) (error 'interp "openbox not implemented")]
    [seqn (expr1 expr2) (error 'interp "seqn not implemented")]))

(define (rinterp expr)
  (interp expr (mtSub) (mtSto)))

(test (rinterp (cparse '3)) (v*s (numV 3) (mtSto)))
(test (rinterp (cparse '{+ 3 4})) (v*s (numV 7) (mtSto)))
(test (rinterp (cparse '{+ {- 3 4} 7})) (v*s (numV 6) (mtSto)))
(test (rinterp (cparse '{with {{x {+ 5 5}}} {+ x x}})) (numV 20))
(test (rinterp (cparse '{with {{x 5}} {+ x x}})) (numV 10))
(test (rinterp (cparse '{with {{x {+ 5 5}}} {with {{y {- x 3}}} {+ y y}}})) (numV 14))
(test (rinterp (cparse '{with {{x 5} {y {- 5 3}}} {+ x y}})) (numV 7))
(test (rinterp (cparse '{with {{x 5}} {+ x {with {{x 3}} 10}}})) (numV 15))
(test (rinterp (cparse '{with {{x 5}} {+ x {with {{x 3}} x}}})) (numV 8))
(test (rinterp (cparse '{with {{x 5}} {+ x {with {{y 3}} x}}})) (numV 10))
(test (rinterp (cparse '{with {{x 5}} {with {{y x}} y}})) (numV 5))
(test (rinterp (cparse '{with {{x 5}} {with {{x x}} x}})) (numV 5))
(test (rinterp (cparse '{{fun {x} x} 3})) (numV 3))
(test (rinterp (cparse '{{{fun {x} x} {fun {x} {+ x 5}}} 3})) (numV 8))
(test (rinterp (cparse '{with {{x 3}} {fun {y} {+ x y}}})) (closureV '(y) (binop + (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub))))
(test (rinterp (cparse '{with {{x 10}} {{fun {y} {+ y x}} {+ 5 x}}})) (numV 25))
(test (rinterp (cparse '{with {{x 1} {y 2} {z 3}} {+ {+ x y} z}})) (numV 6))
(test (rinterp (cparse '{{fun {x y z} {+ {+ x y} z}} 1 2 3})) (numV 6))
(test (rinterp (cparse '{if0 {+ 0 0} 0 1})) (numV 0))
(test (rinterp (cparse '{if0 {+ 0 -5} 1 1})) (numV 1))
(test (rinterp (cparse '{with {{f {fun {x} {+ x x}}}} {if0 {- 5 {+ 2 3}} {f 2} {f 3}}})) (numV 4))
(test (rinterp (cparse '{with {{f {fun {x} {+ x x}}}} {if0 {- 5 {+ 2 4}} {f 2} {f 3}}})) (numV 6))
(test (rinterp (cparse '{with {{Y {fun {le} {{fun {f} {f f}} {fun {f} {le {fun {x} {{f f} x}}}}}}}} {{Y {fun {factorial} {fun {n} {if0 n 1 {* n {factorial {- n 1}}}}}}} 6}})) (numV 720))
(test/exn (rinterp (cparse '{with {{fac {fun {n} {if0 n 1 {* n {fac {+ n -1}}}}}}} {fac 5}})) "fac symbol")

(test (rinterp (cparse '{with {{b {newbox 0}}}
                              {if0 {seqn {setbox b 5}
                                         {openbox b}}
                                   1
                                   {openbox b}}})) (numV 5))