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
     (Prog (optimize-ds ds) (optimize-source e (optimize-ds ds)))]))

;; Expr Env Defns -> Answer
(define (optimize-source e ds)
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
    [(Prim1 p e) (optimize-prim1 p (optimize-source e ds))]
    [(Prim2 p e1 e2) (optimize-prim2 p (optimize-source e1 ds) (optimize-source e2 ds))]
    [(Prim3 p e1 e2 e3) (Prim3 p (optimize-source e1 ds) (optimize-source e2 ds) (optimize-source e2 ds))]
    ; optimize p, if its a literal (int, bool, char) just interp.
    ; or really even only if its just a bool, could go one way or the other
    [(If p e1 e2) (optimize-if (optimize-source p ds) (optimize-source e1 ds) (optimize-source e2 ds))]
    [(Begin e1 e2) (Begin (optimize-source e1 ds) (optimize-source e2 ds))]
    [(Let x e1 e2) (Let x (optimize-source e1 ds) (optimize-source e2) ds)]
    [(Lam i xs e) (Lam i xs (optimize-source e ds))]
    ; i need to check here if all es are literals
    ; if all es are literals, can we call the fx?
    [(App e es)
     (match (all-literals (optimize-app-args es ds))
      [#f (App (optimize-source e ds) (optimize-app-args es ds))]
      [#t 
       (match (find-f (optimize-source e ds) ds)
        ['err 'err]
        [d (optimize-app e es d ds)])])]
    [(Match e ps es) (Match (optimize-source e ds) ps (map optimize-source es ds))]))

(define (optimize-app e es d ds)
  (match d
   [(Defn f xs e) (optimize-source e (append xs es) ds)]))

(define (find-f e ds)
 (match e
  [(Var f) 
    (match ds 
     ['() 'err]
     [(cons x xs) 
     (match x
       [(Defn v a e) 
        (match (equal? f v)
         [#t (Defn v a e)]
         [#f (find-f e xs)])])])]
  [_ 'err]))

(define (optimize-ds ds)
  (match ds
   ['() '()]
   [(cons x xs) 
    (match x
     [(Defn v a e) (cons (Defn v a (optimize-source e ds)) (optimize-ds xs))])]))

(define (all-literals es)
  (match es
   ['() #t]
   [(cons x xs) 
    (match x
     [(or (Int v) (Char v) (Bool v) (Str v)) (all-literals xs)]
     [_ #f])]))


(define (optimize-if p e1 e2)
  (match p
   [(Bool (? boolean? v1)) (if v1 e1 e2)]
   [(Int  (? integer? v1)) (if v1 e1 e2)]
   [(Char (? char? v1))    (if v1 e1 e2)]
   [_ (If p e1 e2)]))

;; Value [Listof Pat] [Listof Expr] Env Defns -> Answer
(define (interp-match v ps es r ds)
  (match* (ps es)
    [('() '()) 'err]
    [((cons p ps) (cons e es))
     (match (interp-match-pat p v r)
       [#f (interp-match v ps es r ds)]
       [r  (optimize-source e ds)])]))

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
(define (optimize-app-args es ds)
  (match es
    ['() '()]
    [(cons e es)
     (match (optimize-source e ds)
       ['err 'err]
       [v (match (optimize-app-args es ds)
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
