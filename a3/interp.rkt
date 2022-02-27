#lang racket
(provide interp)
(require "ast.rkt" "interp-prim.rkt")

;; type Value =
;; | Integer
;; | Boolean

;; Expr -> Value
(define (interp e)
  (match e
    [(Int i) i]
    [(Bool b) b]
    [(Prim1 p e)
     (interp-prim1 p (interp e))]
    [(If e1 e2 e3)
     (if (interp e1)
         (interp e2)
         (interp e3))]
    ;; TODO: Handle cond
    [(Cond e1 e2)
     (let ((c1 (interp-clauses e1))) 
       (if c1
         c1
         (interp e2)))]
    )
    ;; TODO: Handle case
)

(define (interp-clauses e)
  (match e
    [(cons x xs) (match x
                   [(Clause e1 e2) (if (interp e1)
                          (interp e2)
                          (interp-clauses xs))]
                   ['() #f])]
    ['() #f]))

