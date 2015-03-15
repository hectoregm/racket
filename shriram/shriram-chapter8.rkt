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
