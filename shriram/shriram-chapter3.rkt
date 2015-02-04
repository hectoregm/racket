#lang plai

(print-only-errors true)

(define-type WAE
  [num (n number?)]
  [add (lhs WAE?)
       (rhs WAE?)]
  [sub (lhs WAE?)
       (rhs WAE?)]
  [with (name symbol?)
        (named-expr WAE?)
        (body WAE?)]
  [id (name symbol?)])

(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(list? sexp)
     (case (first sexp)
       [(+) (add (parse (second sexp))
                 (parse (third sexp)))]
       [(-) (sub (parse (second sexp))
                 (parse (third sexp)))])]))

(define (subst expr sub-id val)
  (type-case WAE expr
    [num (n) expr]
    [add (l r) (add (subst l sub-id val)
                    (subst r sub-id val))]
    [sub (l r) (sub (subst l sub-id val)
                    (subst r sub-id val))]
    [with (bound-id named-expr bound-body)
          (if (symbol=? bound-id sub-id)
              expr
              (with bound-id
                    named-expr
                    (subst bound-body sub-id val)))]
    [id (v) (if (symbol=? v sub-id) val expr)]))


;; calc : AE -> number
;;(define (calc an-ae)
;;  (type-case AE an-ae
;;    [num (n) n]
;;    [add (l r) (+ (calc l) (calc r))]
;;    [sub (l r) (- (calc l) (calc r))]))

;;(test (calc (parse '3)) 3)
;;(test (calc (parse '{+ 3 4})) 7)
;;(test (calc (parse '{+ {- 3 4} 7})) 6)