#lang racket
(require "ast.rkt")
(provide intern)

;; Expr -> Expr
(define (intern e)
 ; e)
  (nest-lets (replace-strs e) (get-literals e)))

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
      (append strs (get-strs e strs))]
    [(Prim2 p e1 e2)
      (append strs (get-strs e1 strs) (get-strs e2 strs))]
    [(Prim3 p e1 e2 e3)
      (append (get-strs e1 strs) (get-strs e2 strs) (get-strs e3 strs))]
    [(If p e1 e2)
      (append strs (get-strs e1 strs) (get-strs e2 strs))]
    [(Begin e1 e2)
      (append strs (get-strs e1 strs) (get-strs e2 strs))]
    [(Let x e1 e2)
      (append strs (get-strs e1 strs) (get-strs e2 strs))]))

