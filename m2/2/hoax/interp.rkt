#lang racket
(provide interp interp-env interp-prim1 translate-literals nest-lets replace-strs get-literals)
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

(define (interp e)
 (interp-env e '()))

;; Expr -> Answer
(define (translate-literals e)
  (nest-lets (replace-strs e) (get-literals e)))

(define (nest-lets e strs)
 (match strs
  ['() e]
  [(cons x xs) (Let x (Str (symbol->string x)) (nest-lets e xs))]))

(define (get-literals e)
 ; breaks on empty list maybe
 ; i don't know if compile can handle these gensyms
   (remove-duplicates (get-strs e '())))




;; Expr Env -> Answer
(define (replace-strs e)
  (match e
    [(Int i)  (Int i)]
    [(Bool b) (Bool b)]
    [(Char c) (Char c)]
    [(Eof)    (Eof)]
    [(Empty)  (Empty)]
    [(Var x)  (Var x)]
    [(Str s)  (Var (string->symbol s))] 
    [(Prim0 'void) (Prim0 'void)]
    [(Prim0 'read-byte) (Prim0 'read-byte)]
    [(Prim0 'peek-byte) (Prim0 'peek-byte)]
    [(Prim1 p e)
      (Prim1 p (replace-strs e))]
    [(Prim2 p e1 e2)
      (Prim2 p (replace-strs e1) (replace-strs e2))]
    [(Prim3 p e1 e2 e3)
      (Prim3 p (replace-strs e1) (replace-strs e2) (replace-strs e3))]
    [(If p e1 e2)
      (If p (replace-strs e1) (replace-strs e2))]
    [(Begin e1 e2)
      (Begin (replace-strs e1) (replace-strs e2))]
    [(Let x e1 e2)
      (Let x (replace-strs e1) (replace-strs e2))]))

;; Expr Env -> Answer
(define (get-strs e strs)
  (match e
    [(Int i)  '()]
    [(Bool b) '()]
    [(Char c) '()]
    [(Eof)    '()]
    [(Empty)  '()]
    [(Var x)  '()]
    [(Str s)  (cons (string->symbol s) '())] 
    [(Prim0 'void) '()]
    [(Prim0 'read-byte) '()]
    [(Prim0 'peek-byte) '()]
    [(Prim1 p e)
      (append (get-strs e strs) strs)]
    [(Prim2 p e1 e2)
      (append (get-strs e1 strs) (get-strs e2 strs) strs)]
    [(Prim3 p e1 e2 e3)
      (append strs (get-strs e1 strs) (get-strs e2 strs) (get-strs e3 strs))]
    [(If p e1 e2)
      (append strs (get-strs e1 strs) (get-strs e2 strs))]
    [(Begin e1 e2)
      (append strs (get-strs e1 strs) (get-strs e2 strs))]
    [(Let x e1 e2)
      (append strs (get-strs e1 strs) (get-strs e2 strs))]))

;; Expr Env -> Answer
(define (interp-env e r)
  (match e
    [(Int i)  i]
    [(Bool b) b]
    [(Char c) c]
    [(Eof)    eof]
    [(Empty)  '()]
    [(Var x)  (lookup r x)]
    [(Str s)  s] 
    [(Prim0 'void) (void)]
    [(Prim0 'read-byte) (read-byte)]
    [(Prim0 'peek-byte) (peek-byte)]
    [(Prim1 p e)
     (match (interp-env e r)
       ['err 'err]
       [v (interp-prim1 p v)])]
    [(Prim2 p e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v1 (match (interp-env e2 r)
             ['err 'err]
             [v2 (interp-prim2 p v1 v2)])])]
    [(Prim3 p e1 e2 e3)
     (match (interp-env e1 r)
       ['err 'err]
       [v1 (match (interp-env e2 r)
             ['err 'err]
             [v2 (match (interp-env e3 r)
                   ['err 'err]
                   [v3 (interp-prim3 p v1 v2 v3)])])])]
    [(If p e1 e2)
     (match (interp-env p r)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r)
            (interp-env e2 r))])]
    [(Begin e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [_    (interp-env e2 r)])]
    [(Let x e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v (interp-env e2 (ext r x v))])]))

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
