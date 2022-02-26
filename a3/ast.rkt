#lang racket
(provide (all-defined-out))

;; TODO: Add other forms of expression to the type and structure
;; definitions

;; type Expr =
;; | (Int Integer)
;; | (Bool Boolean)
;; | (Prim1 Op Expr)
;; | (If Expr Expr Expr)


;; type Op = 'abs | 'not | '- | 'add1 | 'sub1 | 'zero?



(struct Int (i)            #:prefab)
(struct Bool (b)           #:prefab)
(struct Prim1 (p e)        #:prefab)
(struct If (e1 e2 e3)      #:prefab)
(struct Cond (cs e)        #:prefab)
(struct Case (e cs el)     #:prefab)
(struct Clause (p b)       #:prefab)


