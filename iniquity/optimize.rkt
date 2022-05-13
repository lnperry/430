#lang racket
(provide optimize optimize-source)
(require "ast.rkt"
         "env.rkt")

(define (optimize p)
  (match p
    [(Prog ds e)
     (Prog (optimize-ds ds) (optimize-source e))]))

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
    [(Prim1 p e) (static-prim1 p (optimize-source e))]
    [(Prim2 p e1 e2) (static-prim2 p (optimize-source e1) (optimize-source e2))]
    [(Prim3 p e1 e2 e3) (Prim3 p (optimize-source e1) (optimize-source e2) (optimize-source e3))]
    [(If p e1 e2) (optimize-if (optimize-source p) (optimize-source e1) (optimize-source e2))]
    [(Begin e1 e2) (Begin (optimize-source e1) (optimize-source e2))]
    [(Let x e1 e2) (optimize-let x (optimize-source e1) (optimize-source e2))]
    [(App e es) (App e (map optimize-source es))]))

(define (static-prim1 p1 v)
  (match (list p1 v)
    [(list 'add1  (Int  v1))                                      (Int (add1 v1))]
    [(list 'sub1  (Int  v1))                                      (Int (sub1 v1))]
    [(list 'zero? (Int  v1))                                      (Bool (zero? v1))]
    [(list 'char? (or (Bool v1) (Int v1) (Char v1) (Str v1)))     (Bool (char? v1))]
    [(list 'char->integer (Char (? char? v1)))                    (Int (char->integer v1))]
    [(list 'integer->char (Int  (? codepoint? v1)))               (Char (integer->char v1))]
    [(list 'empty? (or (Bool v1) (Int v1) (Char v1) (Str v1)))    (Bool (empty? v1))]
    [(list 'cons? (or (Bool v1) (Int v1) (Char v1) (Str v1)))     (Bool (cons? v1))]
    [(list 'box? (or (Bool v1) (Int v1) (Char v1) (Str v1)))      (Bool (box? v1))]
    [(list 'vector? (or (Bool v1) (Int v1) (Char v1) (Str v1)))   (Bool (vector? v1))]
    [(list 'string? (or (Bool v1) (Int v1) (Char v1) (Str v1)))   (Bool (string? v1))]
    [(list 'car v1) 
      (match v1 
       [(Prim2 'cons e1 (? optimize-safe?)) (optimize-source e1)]
       [_ (Prim1 p1 v)])]
    [(list 'cdr v1) 
      (match v1 
       [(Prim2 'cons (? optimize-safe?) e2) (optimize-source e2)]
       [_ (Prim1 p1 v)])]
    [(list 'unbox v1) 
      (match v1 
       [(Prim1 'box (? optimize-safe? e1)) (optimize-source e1)]
       [_ (Prim1 p1 v)])]
    [_                                                            (Prim1 p1 v)]))

(define (static-prim2 p v1 v2)
  (match (list p v1 v2)
    [(list '+ (Int (? integer? v1)) (Int (? integer? v2)))  (Int (+ v1 v2))]
    [(list '- (Int (? integer? v1)) (Int (? integer? v2)))  (Int (- v1 v2))]
    [(list '< (Int (? integer? v1)) (Int (? integer? v2)))  (Bool (< v1 v2))]
    [(list '= (Int (? integer? v1)) (Int (? integer? v2)))  (Bool (= v1 v2))]
    [_ (Prim2 p v1 v2)]))

(define (optimize-let x e1 e2)
 (match (bindings x e2)
  [0 (if (optimize-let-safe? x e1)
      e2
      (Let x e1 e2))] ; replace with (begin)s?
  [1 (if (and (optimize-let-safe? x e1) (optimize-let-safe? x e2))
      (optimize-source (replace-x x e1 e2))
      (Let x e1 e2))]
  [_ (Let x e1 e2)]))

(define (bindings v e)
  (match e
    [(Int i)  0]
    [(Bool b) 0]
    [(Char c) 0]
    [(Eof)    0]
    [(Empty)  0]
    [(Var x)  (if (eq? v x) 1 0)]
    [(Str s)  0]
    [(Prim0 'void) 0]
    [(Prim0 'read-byte) 0]
    [(Prim0 'peek-byte) 0]
    [(Prim1 p e) (+ (bindings v e))]
    [(Prim2 p e1 e2) (+ (bindings v e1) (bindings v e2))]
    [(Prim3 p e1 e2 e3) (+ (bindings v e1) (bindings v e2) (bindings v e3))]
    [(If p e1 e2) (+ (bindings v p) (bindings v e1) (bindings v e2))]
    [(Begin e1 e2) (+ (bindings v e1) (bindings v e2))]
    [(Let x e1 e2) (+ (bindings v e1) (bindings v e2))]
    [(App e es) (+ (bindings e) (foldl + 0 (map bindings es)))]))

(define (replace-x var val expr)
  (match expr
    [(Int i)  (Int i)]
    [(Bool b) (Bool b)]
    [(Char c) (Char c)]
    [(Eof)    (Eof)]
    [(Empty)  (Empty)]
    [(Var x)  (if (eq? var x) val (Var x))]
    [(Str s)  (Str s)]
    [(Prim0 'void) expr]
    [(Prim0 'read-byte) expr]
    [(Prim0 'peek-byte) expr]
    [(Prim1 p e) (Prim1 p (replace-x var val e))]
    [(Prim2 p e1 e2) (Prim2 p (replace-x var val e1) (replace-x var val e2))]
    [(Prim3 p e1 e2 e3) (Prim3 p (replace-x var val e1) (replace-x var val e2) (replace-x var val e3))]
    [(If p e1 e2) (If (replace-x var val p) (replace-x var val e1) (replace-x var val e2))]
    [(Begin e1 e2) (Begin (replace-x var val e1) (replace-x var val e2))]
    [(Let x e1 e2) (Let x (replace-x var val e1) (replace-x var val e2))]
    [(App e es) (App (bindings e) (map (lambda (x) (replace-x var val x)) es))]))

(define (optimize-safe? e)
  (match e
    [(Int i)  #t]
    [(Bool b) #t]
    [(Char c) #t]
    [(Eof)    #f] ;i/o unsafe
    [(Empty)  #t]
    ;unsafe, can throw exception that would be optimized away
    [(Var x)  #f] 
    [(Str s)  #t]
    [(Prim0 'void) #t] 
    [(Prim0 'read-byte) #f] ;i/o unsafe
    [(Prim0 'peek-byte) #f] ;i/o unsafe
    [(Prim1 p e) (optimize-safe? e)]
    [(Prim2 p e1 e2) (and (optimize-safe? e1) (optimize-safe? e2))]
    [(Prim3 p e1 e2 e3) (and (optimize-safe? e1) (optimize-safe? e2) (optimize-safe? e3))]
    [(If p e1 e2) (and (optimize-safe? p) (optimize-safe? e1) (optimize-safe? e2))]
    [(Begin e1 e2) (and (optimize-safe? e1) (optimize-safe? e2))]
    [(Let x e1 e2) (and (optimize-safe? e1) (optimize-safe? e2))]
    [(App e es) (and (optimize-safe? e) (andmap optimize-safe? es))]))

(define (optimize-let-safe? v expr)
  (match expr
    [(Int i)  #t]
    [(Bool b) #t]
    [(Char c) #t]
    [(Eof)    #f] ;i/o unsafe
    [(Empty)  #t]
    ;unsafe, can throw exception that would be optimized away
    [(Var x)  (eq? v x)] 
    [(Str s)  #t]
    [(Prim0 'void) #f] ;i/o unsafe
    [(Prim0 'read-byte) #f] ;i/o unsafe
    [(Prim0 'peek-byte) #f] ;i/o unsafe
    [(Prim1 p e) (optimize-let-safe? v e)]
    [(Prim2 p e1 e2) (and (optimize-let-safe? v e1) (optimize-let-safe? v e2))]
    [(Prim3 p e1 e2 e3) (and (optimize-let-safe? v e1) 
                             (optimize-let-safe? v e2) 
                             (optimize-let-safe? v e3))]
    [(Prim3 p e1 e2 e3) #f]
    [(If p e1 e2) (and (optimize-let-safe? v p) 
                       (optimize-let-safe? v e1) 
                       (optimize-let-safe? v e2))]
    [(Begin e1 e2) (and (optimize-let-safe? v e1) (optimize-let-safe? v e2))]
    [(Let x e1 e2) (and (optimize-let-safe? v e1) (optimize-let-safe? v e2))]
    [(App e es) (and (optimize-let-safe? v e) (andmap (lambda (x) (optimize-let-safe? v)) es))]))

(define (optimize-ds ds)
  (match ds
   ['() '()]
   [(cons x xs) 
    (match x
     [(Defn v a e) (cons (Defn v a (optimize-source e)) (optimize-ds xs))])]))

(define (optimize-if p e1 e2)
  (match p
   [(Bool (? boolean? v1)) (if v1 e1 e2)]
   [(Int  (? integer? v1)) (if v1 e1 e2)]
   [(Char (? char? v1))    (if v1 e1 e2)]
   [_ (If p e1 e2)]))

;; (Listof Expr) REnv Defns -> (Listof Value) | 'err
(define (optimize-app-args es)
  (match es
    ['() '()]
    [(cons e es)
     (match (optimize-source e)
       ['err 'err]
       [v (match (optimize-app-args es)
            ['err 'err]
            [vs (cons v vs)])])]))

(define (codepoint? v)
  (and (integer? v)
       (or (<= 0 v 55295)
           (<= 57344 v 1114111))))
