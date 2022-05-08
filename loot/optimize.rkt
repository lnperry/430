#lang racket
(provide optimize optimize-source)
(require "ast.rkt"
         "env.rkt"
         "optimize-prims.rkt")

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
;; | (Value ... -> Answer)

;; type REnv = (Listof (List Id Value))
;; type Defns = (Listof Defn)

;; Prog -> Answer
(define (optimize p)
  (match p
    [(Prog ds e)
     (Prog ds (optimize-source e))]))

;; Expr Env Defns -> Answer
(define (optimize-source e)
  (match e
    [(Int i)  (Int i)]
    [(Bool b) (Bool b)]
    [(Char c) (Char c)]
    [(Eof)    (Eof)]
    [(Empty)  (Empty)]
    [(Var x)  (Var x)]
    [(Str s)  (Str s)]
    [(Prim0 'void) (Prim0 'void)]
    [(Prim0 'read-byte) (Prim0 'read-byte)]
    [(Prim0 'peek-byte) (Prim0 'peek-byte)]
    [(Prim1 p e) (Prim1 p (optimize-prim1 e))]
    [(Prim2 p e1 e2) (Prim2 p (optimize-source e1) (optimize-source e2))]
    [(Prim3 p e1 e2 e3) (Prim3 p (optimize-source e1) (optimize-source e2) (optimize-source e2))]
    [(If p e1 e2) (If p (optimize-source e1) (optimize-source e2))]
    [(Begin e1 e2) (Begin (optimize-source e1) (optimize-source e2))]
    [(Let x e1 e2) (Let (optimize-source e1) (optimize-source e2))]
    [(Lam i xs e) (Lam i xs (optimize-source e))]
    [(App e es) (App e es)]
    [(Match e ps es) (Match (optimize-source e) (optimize-source ps) (optimize-source es))]))

;; Value [Listof Pat] [Listof Expr] Env Defns -> Answer
(define (interp-match v ps es r ds)
  (match* (ps es)
    [('() '()) 'err]
    [((cons p ps) (cons e es))
     (match (interp-match-pat p v r)
       [#f (interp-match v ps es r ds)]
       [r  (optimize-source e)])]))

;; Pat Value Env -> [Maybe Env]
(define (interp-match-pat p v r)
  (match p
    [(PWild) r]
    [(PVar x) (ext r x v)]
    [(PLit l) (and (eqv? l v) r)]
    [(PBox p)
     (match v
       [(box v)
        (interp-match-pat p v r)]
       [_ #f])]
    [(PCons p1 p2)
     (match v
       [(cons v1 v2)
        (match (interp-match-pat p1 v1 r)
          [#f #f]
          [r1 (interp-match-pat p2 v2 r1)])]
       [_ #f])]
    [(PAnd p1 p2)
     (match (interp-match-pat p1 v r)
       [#f #f]
       [r1 (interp-match-pat p2 v r1)])]))

;; Id Env [Listof Defn] -> Answer
(define (interp-var x r ds)
  (match (lookup r x)
    ['err (match (defns-lookup ds x)
            [(Defn f xs e) (optimize-source (Lam f xs e) '() ds)]
            [#f 'err])]
    [v v]))

;; (Listof Expr) REnv Defns -> (Listof Value) | 'err
(define (optimize-source* es r ds)
  (match es
    ['() '()]
    [(cons e es)
     (match (optimize-source e r ds)
       ['err 'err]
       [v (match (optimize-source* es r ds)
            ['err 'err]
            [vs (cons v vs)])])]))

;; Defns Symbol -> [Maybe Defn]
(define (defns-lookup ds f)
  (findf (match-lambda [(Defn g _ _) (eq? f g)])
         ds))

(define (zip xs ys)
  (match* (xs ys)
    [('() '()) '()]
    [((cons x xs) (cons y ys))
     (cons (list x y)
           (zip xs ys))]))
