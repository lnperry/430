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
       (if cls
         cls
         (interp e)))]
    [(Case e cs el)
     (let ((cls (interp-case-clauses (interp e) cs))) 
       (if (equal? #f cls)
         (interp el)
         cls))]
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
		      ;; if e2 is false, then this will break bc it will return the else
                      (interp e2) 
                      (interp-cond-clauses xs))]
                   ['() #f])]
    ['() #f]))


(define (interp-case-clauses m e)
  (match e
    [(cons x xs) (match x
                   [(Clause e1 e2) 
                    (let ((matchval (find-clause-match m e1 e2)))
                     (if (equal? #f matchval)
                       matchval ;; no matches in this leftClause list
                       (interp e2)))]
                   ['() '()])]
    ['() #f]))

(define (find-clause-match m e1 e2)
  (match e1
    [(cons x xs) (if (equal? m (interp x))
                  e2 ;; if returned, interp in fx above
                   (find-clause-match m xs e2))]
                   ;;(list "m" m "e1" e1 "x" x "xs" xs "equal?" (equal? m (interp x)))]
    ['() #f]))
