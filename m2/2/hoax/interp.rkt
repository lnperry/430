#lang racket
(provide interp interp-env interp-prim1 get-literals get-strs)
(require "ast.rkt"
         "env.rkt"
         "interp-prims.rkt")

;; type Answer = Value | 'err

;; type Value =
;; | Integer
;; | Boolean
;; | Character
;; | Eof
;; | Void
;; | '()
;; | (cons Value Value)
;; | (box Value)
;; | (vector Value ...)
;; | (string Char ...)

;; type REnv = (Listof (List Id Value))

(define x '())

;; Expr -> Answer
(define (interp e)
  (replace-strs e))

(define (get-literals e)
   (get-strs e '()))


;; Expr Env -> Answer
(define (replace-strs e)
  (match e
    [(Int i)  (Int i)]
    [(Bool b) (Bool b)]
    [(Char c) (Char c)]
    [(Eof)    (Eof)]
    [(Empty)  (Empty)]
    [(Var x)  (Var x)]
    [(Str s)  "isstring"] 
    [(Prim0 'void) (Prim0 'void)]
    [(Prim0 'read-byte) (Prim0 'read-byte)]
    [(Prim0 'peek-byte) (Prim0 'peek-byte)]
    [(Prim1 p e)
      (Prim1 p (interp-env e))]
    [(Prim2 p e1 e2)
      (Prim2 p (interp-env e1) (interp-env e2))]
    [(Prim3 p e1 e2 e3)
      (Prim3 p (interp-env e1) (interp-env e2) (interp-env e3))]
    [(If p e1 e2)
      (If p (interp-env e1) (interp-env e2))]
    [(Begin e1 e2)
      (Begin (interp-env e1) (interp-env e2))]
    [(Let x e1 e2)
      (Let x (interp-env e1) (interp-env e2))]))

;; Expr Env -> Answer
(define (get-strs e strs)
  (match e
    [(Int i)  '()]
    [(Bool b) '()]
    [(Char c) '()]
    [(Eof)    '()]
    [(Empty)  '()]
    [(Var x)  '()]
    [(Str s)  (cons s strs)] 
    [(Prim0 'void) '()]
    [(Prim0 'read-byte) '()]
    [(Prim0 'peek-byte) '()]
    [(Prim1 p e)
      (cons strs (get-strs e strs))]
    [(Prim2 p e1 e2)
      (append strs (get-strs e1 strs) (get-strs e2 strs))]
    [(Prim3 p e1 e2 e3)
      (append strs (get-strs e1 strs) (get-strs e2 strs) (get-strs e3 strs))]
    [(If p e1 e2)
      (append strs (get-strs e1 strs) (get-strs e2 strs))]
    [(Begin e1 e2)
      (append strs (get-strs e1 strs) (get-strs e2 strs))]
    [(Let x e1 e2)
      (append strs (get-strs e1 strs) (get-strs e2 strs))]))

;; Expr Env -> Answer
(define (interp-env e)
  (match e
    [(Int i)  (Int i)]
    [(Bool b) (Bool b)]
    [(Char c) (Char c)]
    [(Eof)    (Eof)]
    [(Empty)  (Empty)]
    [(Var x)  (Var x)]
    [(Str s)  "isstring"] 
    [(Prim0 'void) (Prim0 'void)]
    [(Prim0 'read-byte) (Prim0 'read-byte)]
    [(Prim0 'peek-byte) (Prim0 'peek-byte)]
    [(Prim1 p e)
      (Prim1 p (interp-env e))]
    [(Prim2 p e1 e2)
      (Prim2 p (interp-env e1) (interp-env e2))]
    [(Prim3 p e1 e2 e3)
      (Prim3 p (interp-env e1) (interp-env e2) (interp-env e3))]
    [(If p e1 e2)
      (If p (interp-env e1) (interp-env e2))]
    [(Begin e1 e2)
      (Begin (interp-env e1) (interp-env e2))]
    [(Let x e1 e2)
      (Let x (interp-env e1) (interp-env e2))]))

(define (lookup env x)
  (match env
    ['() 'err]
    [(cons (list y i) env)
     (match (symbol=? x y)
       [#t i]
       [#f (lookup env x)])]))

;; Env Variable Value -> Value
(define (ext r x i)
  (cons (list x i) r))
