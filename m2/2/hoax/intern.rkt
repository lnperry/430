#lang racket
(require "ast.rkt")
(provide intern)


;; Expr -> Expr
(define (intern e)
 (let ((lits (convert-literals (get-literals e))))
  (nest-lets (replace-strs e lits) lits)))


;; Expr -> Answer
(define (translate-literals e)
  (nest-lets (replace-strs e) (convert-literals (get-literals e))))

(define (str->sym str strs)
 (match strs
  [(cons x (cons y zs)) (if (string=? str x)
                   y
                   (str->sym str zs))]))
  
(define (sym->str str strs)
 (match strs
  [(cons x (cons y zs)) (if (eq? str y)
                   x
                   (str->sym str zs))]))

(define (nest-lets e lits)
 (match lits
  ['() e]
  [(cons x (cons y zs)) (Let y (Str x) (nest-lets e zs))]))

(define (get-literals e)
 ; breaks on empty list maybe
 ; i don't know if compile can handle these gensyms
   (remove-duplicates (get-strs e '())))

(define (convert-literals strs)
  (match strs
   ['() '()]
   [(cons x xs) (append (list x (gensym x)) (convert-literals xs))]))

; i have list of unique strings
; iterate thru list of unique, gensym, store in new list
; iterate thru get-strs with unique gensym'd things



;; Expr Env -> Answer
(define (replace-strs e lits)
  (match e
    [(Int i)  (Int i)]
    [(Bool b) (Bool b)]
    [(Char c) (Char c)]
    [(Eof)    (Eof)]
    [(Empty)  (Empty)]
    [(Var x)  (Var x)]
    [(Str s)  (Var (str->sym s lits))] 
    [(Prim0 'void) (Prim0 'void)]
    [(Prim0 'read-byte) (Prim0 'read-byte)]
    [(Prim0 'peek-byte) (Prim0 'peek-byte)]
    [(Prim1 p e)
      (Prim1 p (replace-strs e lits))]
    [(Prim2 p e1 e2)
      (Prim2 p (replace-strs e1 lits) (replace-strs e2 lits))]
    [(Prim3 p e1 e2 e3)
      (Prim3 p (replace-strs e1 lits) (replace-strs e2 lits) (replace-strs e3 lits))]
    [(If p e1 e2)
      (If p (replace-strs e1 lits) (replace-strs e2 lits))]
    [(Begin e1 e2)
      (Begin (replace-strs e1 lits) (replace-strs e2 lits))]
    [(Let x e1 e2)
      (Let x (replace-strs e1 lits) (replace-strs e2 lits))]))

;; Expr Env -> Answer
(define (get-strs e strs)
  (match e
    [(Int i)  '()]
    [(Bool b) '()]
    [(Char c) '()]
    [(Eof)    '()]
    [(Empty)  '()]
    [(Var x)  '()]
    [(Str s)  (cons s '())] 
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


