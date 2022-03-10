#lang racket
(require "ast.rkt")
(provide max-stack-size)

(module+ test (require rackunit))

;; max-stack-size : Expr -> Natural
;; Compute the maximum number of elements pushed on the stack
;; when executing the compiled code for the given expression.

(module+ test
  (check-equal? (max-stack-size (Bool #t)) 0)
  (check-equal? (max-stack-size (Var 'x)) 0)
  (check-equal? (max-stack-size (Prim0 'read-byte)) 0)
  (check-equal? (max-stack-size (Prim1 'add1 (Int 5))) 0)
  (check-equal? (max-stack-size (Let 'x (Int 5) (Var 'x))) 1)
  (check-equal? (max-stack-size (Let 'x (Let 'y (Int 5) (Var 'y)) (Var 'x))) 1)  
  (check-equal? (max-stack-size (Prim2 '+ (Int 5) (Int 4))) 1))
   
(define (max-stack-size e)
  (compile-e e)
  )

;; Expr CEnv -> Asm
(define (compile-e e)
  (match e
    [(Int i)            0]
    [(Bool b)           0]
    [(Char c)           0]
    [(Eof)              0]
    [(Var x)            0]
    [(Prim0 p)          0]
    [(Prim1 p e)        (compile-prim1 e)]
    [(Prim2 p e1 e2)    (compile-prim2 e1 e2)]
    [(If e1 e2 e3)      (compile-if e1 e2 e3)]
    [(Begin e1 e2)      (compile-begin e1 e2)]
    [(Let x e1 e2)      (compile-let x e1 e2)]))

;; Op1 Expr CEnv -> Asm
(define (compile-prim1 e)
  (compile-e e))

;; Op2 Expr Expr CEnv -> Asm
(define (compile-prim2 e1 e2)
  (+ (+ (compile-e e1) (compile-e e2)) 1))

;; Expr Expr Expr CEnv -> Asm
(define (compile-if e1 e2 e3)
    (+ (compile-e e1)
         (compile-e e2)
         (compile-e e3)))

;; Expr Expr CEnv -> Asm
(define (compile-begin e1 e2)
  (+ (compile-e e1)
       (compile-e e2)))

;; Id Expr Expr CEnv -> Asm
(define (compile-let x e1 e2)
  ;; can toss out 
  (max (compile-e e1)
       (+ (compile-e e2) 1)))
