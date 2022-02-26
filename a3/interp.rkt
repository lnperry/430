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
     (interp-clauses e1)]
       ;;(cond
         ;;(define something
          ;; (match e1
           ;; [(Clause e1 e2) (list (interp e1) (interp e2))]))
         ;;[else e2])]
       
    )
    ;; TODO: Handle case
)

(define (interp-clauses s)
  (for/list ([i s])
    (match i
      [(Clause e1 e2) (list (interp e1) (interp e2))])))
