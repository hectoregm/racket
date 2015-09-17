#lang plai

(print-only-errors true)

;; Binding - Defines a datatype that represents a given symbol
;; and its associsated RCFAE expresion.
(define-type Binding
  [bind (name symbol?) (val RCFAES?)])

;; RCFAES - Datatype that defines the surface RCFAE language.
(define-type RCFAES
  [numS (n number?)]
  [withS (bindings (listof bind?))
         (body RCFAES?)]
  [idS (name symbol?)]
  [if0S (test RCFAES?)
        (truth RCFAES?)
        (falsity RCFAES?)]
  [funS (params (listof symbol?))
        (body RCFAES?)]
  [appS (fun RCFAES?)
        (args (listof RCFAES?))]
  [binopS (f procedure?)
          (l RCFAES?)
          (r RCFAES?)])

;; RCFAE - Datatype that defines the Core RCFAE language
(define-type RCFAE
  [num (n number?)]
  [id (name symbol?)]
  [if0 (test RCFAE?)
       (truth RCFAE?)
       (falsity RCFAE?)]
  [fun (params (listof symbol?))
       (body RCFAE?)]
  [app (fun RCFAE?)
       (args (listof RCFAE?))]
  [binop (f procedure?)
         (l RCFAE?)
         (r RCFAE?)])

(define-type RCFAE-Value
  [numV (n number?)]
  [closureV (param (listof symbol?))
            (body RCFAE?)
            (env Env?)])

(define (boxed-RCFAE-Value? v)
  (and (box? v)
       (RCFAE-Value? (unbox v))))

(define-type Env
  [mtSub]
  [aSub (name symbol?) 
        (value RCFAE-Value?) 
        (env Env?)]
  [aRecSub (name symbol?)
           (value boxed-RCFAE-Value?)
           (env Env?)])

; FUNCIONES AUXILIARES

;; A::= <number>|<symbol>|listof(<A>)
;; B::= (list <symbol> <A>)
;; parse-bindings: listof(B) -> listof(bind?)
;; "Parsea" la lista de bindings lst en sintaxis concreta
;; mientras revisa la lista de id's en busca de repetidos.
;; (define (parse-bindings lst) 
(define (parse-bindings lst)
  (let ([bindRep (buscaRepetido lst (lambda (e1 e2) (symbol=? (car e1) (car e2))))])
    (if (boolean? bindRep)
        (map (lambda (b) (bind (car b) (parse (cadr b)))) lst)
        (error 'parse-bindings (string-append "El id " (symbol->string (car bindRep)) " está repetido")))))

(define (elige s)
  (case s
    [(+) +]
    [(-) -]
    [(*) *]
    [(/) /]))

;; buscaRepetido: listof(X) (X X -> boolean) -> X
;; Dada una lista, busca repeticiones dentro de la misma
;; usando el criterio comp. Regresa el primer elemento repetido
;; o falso eoc.
;; (define (buscaRepetido l comp) 
(define (buscaRepetido l comp) 
  (cond
    [(empty? l) #f]
    [(member? (car l) (cdr l) comp) (car l)]
    [else (buscaRepetido (cdr l) comp)]))

;; member?: X listof(Y) (X Y -> boolean) -> boolean
;; Determina si x está en l usando "comparador" para
;; comparar x con cada elemento de la lista.
;; (define (member? x l comparador)
(define (member? x l comparador)
  (cond
    [(empty? l) #f]
    [(comparador (car l) x) #t]
    [else (member? x (cdr l) comparador)]))


;; num-zero? RCFAE-Value -> boolean
(define (num-zero? n)
  (zero? (numV-n n)))

;; A::= <number>|<symbol>|listof(<A>)
;; parse: A -> RCFAES
(define (parse sexp)
  (cond
    [(symbol? sexp) (idS sexp)]
    [(number? sexp) (numS sexp)]
    [(list? sexp)
     (case (car sexp)
       [(with) (withS (parse-bindings (cadr sexp)) (parse (caddr sexp)))]
       [(fun) (funS (cadr sexp) (parse (caddr sexp)))]
       [(+ - / *) (binopS (elige (car sexp)) (parse (cadr sexp)) (parse (caddr sexp)))]
       [(if0) (if0S (parse (cadr sexp))
                    (parse (caddr sexp))
                    (parse (cadddr sexp)))]
       [else (appS (parse (car sexp)) (map parse (cdr sexp)))])]))

;; lookup : symbol Env -> RCFAE-Value
(define (lookup name env)
  (type-case Env env
    [mtSub () (error 'lookup (string-append (~a name) " symbol is not in the env"))]
    [aSub (bound-name bound-value rest-env)
          (if (symbol=? bound-name name)
              bound-value
              (lookup name rest-env))]
    [aRecSub (bound-name boxed-bound-value rest-env)
             (if (symbol=? bound-name name)
                 (unbox boxed-bound-value)
                 (lookup name rest-env))]))

;; cyclically-bind-and-interp : symbol RCFAE env ! env 
(define (cyclically-bind-and-interp bound-id named-expr env)
  (local ([define value-holder (box (numV 1729))]
          [define new-env (aRecSub bound-id value-holder env)] [define named-expr-val (interp named-expr new-env)])
    (begin
      (set-box! value-holder named-expr-val) new-env)))


(define (desugar expr)
  (type-case RCFAES expr
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
    [appS (fun args) (app (desugar fun) (map (lambda (arg) (desugar arg)) args))]))

(test (desugar (parse '{+ 3 4})) (binop + (num 3) (num 4)))
(test (desugar (parse '{+ {- 3 4} 7})) (binop + (binop - (num 3) (num 4)) (num 7)))
(test (desugar (parse '{with {{x {+ 5 5}}} x})) (app (fun '(x) (id 'x)) (list (binop + (num 5) (num 5))) ))

(define (cparse sexp)
  (desugar (parse sexp)))

(define (apply-binop fun numa numb)
  (numV (fun (numV-n numa)
             (numV-n numb))))

(define (extend-env params args env)
  (cond
    [(empty? params) env]
    [else (aSub (car params)
                (car args)
                (extend-env (cdr params) (cdr args) env))]))

(define (interp expr env)
  (type-case RCFAE expr
    [num (n) (numV n)]
    [id (v) (lookup v env)]
    [if0 (test truth falsity)
         (if (num-zero? (interp test env))
             (interp truth env)
             (interp falsity env))]
    [fun (params bound-body)
         (closureV params bound-body env)]
    [app (fun-expr args)
         (local ([define fun-val (interp fun-expr env)])
           (interp (closureV-body fun-val)
                   (extend-env (closureV-param fun-val)
                               (map (lambda (arg) (interp arg env)) args)
                               (closureV-env fun-val))))]
    [binop (f l r) (apply-binop f (interp l env) (interp r env))]))

(define (rinterp expr)
  (interp expr (mtSub)))

(test (rinterp (cparse '3)) (numV 3))
(test (rinterp (cparse '{+ 3 4})) (numV 7))
(test (rinterp (cparse '{+ {- 3 4} 7})) (numV 6))
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
;;(test/exn (rinterp (cparse '{with {{fac {fun {n} {if0 n 1 {* n {fac {+ n -1}}}}}}} {fac 5}})) "fac symbol")