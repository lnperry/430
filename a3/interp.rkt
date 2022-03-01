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
    [(Cond cs e)
     (let ((cls (interp-cond-clauses cs))) 
       (if (car cls)
         (interp (cdr cls))
         (interp e)))] ;; wait interp else here "AST long as possible"
    [(Case e cs el)
     (let ((cls (interp-case-clauses (interp e) cs))) 
       (if (car cls)
        (interp (cdr cls))
         (interp el)))]
       ;;(if cls
        ;; (interp el)
         ;;(first cls)))]
         
    )
    ;; TODO: Handle case
)

(define (interp-cond-clauses e)
  (match e
    [(cons x xs) (match x
                   [(Clause e1 e2) 
                    (if (interp e1)
		      ;; consider (If e1 e2 e3) from our language
		      ;; if e2 is false, then this will break bc it will return the else
                      (cons #t e2) 
                      (interp-cond-clauses xs))]
                   ['() (cons #f #f)])]
    ['() (cons #f #f)]))


(define (interp-case-clauses m e)
  (match e
    [(cons x xs) (match x
                   [(Clause e1 e2) 
                    (let ((matchval (find-clause-match m e1 e2)))
                     (if (car matchval) 
                       ;; I think this is the same issue i had in cond, p sure i can check w racket if i want or just wlak thru the code by hand
                       (cons #t (cdr matchval)) 
                       ;; recurse
                       (interp-case-clauses m xs)))]
                   ['() (cons #f #f)])]
    ['() (cons #f #f)]))

(define (find-clause-match m e1 e2)
  (match e1
    [(cons x xs) (if (equal? m (interp x))
                   (cons #t e2);; if returned, interp in fx above
                   (find-clause-match m xs e2))]
    ['() (cons #f #f)]))
