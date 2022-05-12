#lang racket
(provide optimize optimize-static)
(require "ast.rkt"
         "env.rkt")

(define (optimize p)
  (match p
    [(Prog ds e)
     (Prog (optimize-ds ds) (optimize-static e))]))

(define (optimize-static e)
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
    [(Prim1 p e) (static-prim1 p (optimize-static e))]
    [(Prim2 p e1 e2) (static-prim2 p (optimize-static e1) (optimize-static e2))]
    ; need to optimize prim3
    [(Prim3 p e1 e2 e3) (Prim3 p (optimize-static e1) (optimize-static e2) (optimize-static e3))]
    [(If p e1 e2) (optimize-if (optimize-static p) (optimize-static e1) (optimize-static e2))]
    [(Begin e1 e2) (Begin (optimize-static e1) (optimize-static e2))]
    [(Let x e1 e2) (optimize-let x (optimize-static e1) (optimize-static e2))]
    [(App e es) (App (optimize-static e) (optimize-app-args es))]))

(define (optimize-let x e1 e2)
  (if 
   (and (eq? 1 (bindings x e2)) (optimize-let-safe? e2) (optimize-let-safe? e1))
    (optimize-static (replace-x x e1 e2)) 
    (Let x e1 e2)))

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

;; Op1 Value -> Answer
(define (static-prim1 p1 v)
  (match (list p1 v)
    [(list 'add1  (Int (? integer? v1)))                          (Int (add1 v1))]
    [(list 'sub1  (Int (? integer? v1)))                          (Int (sub1 v1))]
    [(list 'zero? (Int (? integer? v1)))                          (Bool (zero? v1))]
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
       [(Prim2 'cons e1 (? optimize-safe?)) (optimize-static e1)]
       [_ (Prim1 p1 v)])]
    [(list 'cdr v1) 
      (match v1 
       [(Prim2 'cons (? optimize-safe?) e2) (optimize-static e2)]
       [_ (Prim1 p1 v)])]
    [(list 'unbox v1) 
      (match v1 
       [(Prim1 'box e1) (optimize-static e1)]
       [_ (Prim1 p1 v)])]
    [_                                                            (Prim1 p1 v)]))

;; Op2 Value Value -> Answer
(define (static-prim2 p v1 v2)
  (match (list p v1 v2)
    [(list '+ (Int (? integer? v1)) (Int (? integer? v2)))  (Int (+ v1 v2))]
    [(list '- (Int (? integer? v1)) (Int (? integer? v2)))  (Int (- v1 v2))]
    [(list '< (Int (? integer? v1)) (Int (? integer? v2)))  (Bool (< v1 v2))]
    [(list '= (Int (? integer? v1)) (Int (? integer? v2)))  (Bool (= v1 v2))]
    ; optimize more prim2s?
    [_ (Prim2 p v1 v2)]))

(define (optimize-safe? e)
  (match e
    [#t #t]
    [#f #f]
    [(Int i)  #t]
    [(Bool b) #t]
    [(Char c) #t]
    [(Eof)    #f] ;i/o unsafe
    [(Empty)  #t]
    ;unsafe, can throw exception that would be optimized away
    [(Var x)  #f] 
    [(Str s)  #t]
    [(Prim0 'void) #f] ;i/o unsafe
    [(Prim0 'read-byte) #f] ;i/o unsafe
    [(Prim0 'peek-byte) #f] ;i/o unsafe
    [(Prim1 p e) (prim1-safe p optimize-safe? e)]
    [(Prim2 p e1 e2) (prim2-safe p (optimize-safe? e1) (optimize-safe? e2))]
    [(Prim3 p e1 e2 e3) #f]
    [(If p e1 e2) (and (optimize-safe? p) (optimize-safe? e1) (optimize-safe? e2))]
    [(Begin e1 e2) (and (optimize-safe? e1) (optimize-safe? e2))]
    [(Let x e1 e2) (and (optimize-safe? e1) (optimize-safe? e2))]
    [(App e es) (and (optimize-safe? e) (andmap optimize-safe? es))]))

(define (optimize-let-safe? e)
  (match e
    [#t #t]
    [#f #f]
    [(Int i)  #t]
    [(Bool b) #t]
    [(Char c) #t]
    [(Eof)    #f] ;i/o unsafe
    [(Empty)  #t]
    ;unsafe, can throw exception that would be optimized away
    [(Var x)  #t] 
    [(Str s)  #t]
    [(Prim0 'void) #f] ;i/o unsafe
    [(Prim0 'read-byte) #f] ;i/o unsafe
    [(Prim0 'peek-byte) #f] ;i/o unsafe
    [(Prim1 p e) (prim1-safe p optimize-let-safe? e)]
    [(Prim2 p e1 e2) (prim2-safe p (optimize-let-safe? e1) (optimize-let-safe? e2))]
    [(Prim3 p e1 e2 e3) #f]
    [(If p e1 e2) (and (optimize-safe? p) (optimize-let-safe? e1) (optimize-let-safe? e2))]
    [(Begin e1 e2) (and (optimize-let-safe? e1) (optimize-let-safe? e2))]
    [(Let x e1 e2) (and (optimize-let-safe? e1) (optimize-let-safe? e2))]
    [(App e es) (and (optimize-let-safe? e) (andmap optimize-let-safe? es))]))

;; Op1 Value -> Answer
(define (prim1-safe p1 v)
  (match (list p1 v)
    [(list 'add1  (Int (? integer? v1)))                         #t] 
    [(list 'sub1  (Int (? integer? v1)))                         #t] 
    [(list 'zero? (Int (? integer? v1)))                         #t] 
    [(list 'char? (or (Bool v1) (Int v1) (Char v1) (Str v1)))    #t] 
    [(list 'char->integer (Char (? char? v1)))                   #t] 
    [(list 'integer->char (Int  (? codepoint? v1)))              #t] 
    [(list 'empty? (or (Bool v1) (Int v1) (Char v1) (Str v1)))   #t] 
    [(list 'cons? (or (Bool v1) (Int v1) (Char v1) (Str v1)))    #t] 
    [(list 'box? (or (Bool v1) (Int v1) (Char v1) (Str v1)))     #t] 
    [(list 'vector? (or (Bool v1) (Int v1) (Char v1) (Str v1)))  #t] 
    ; need to check if more prim1s are safe?
    [_                                                           #f]))



(define (prim2-safe p v1 v2)
  (match (list p v1 v2)
   ; need to add all these prim2s are safe
    ; [(list '+ (Int (? integer? v1)) (Int (? integer? v2)))  (Int (+ v1 v2))]
    ; [(list '- (Int (? integer? v1)) (Int (? integer? v2)))  (Int (- v1 v2))]
    ; [(list '< (Int (? integer? v1)) (Int (? integer? v2)))  (Bool (< v1 v2))]
    ; [(list '= (Int (? integer? v1)) (Int (? integer? v2)))  (Bool (= v1 v2))]
    [(list 'cons v1 v2)                   (and (optimize-safe? v1) (optimize-safe? v2))]
    ; [(list 'eq? v1 v2)                    (eq? v1 v2)]
    ; [(list 'make-vector (? integer?) _)
     ; (if (<= 0 v1)
         ; (make-vector v1 v2)
        ; 'err)]
    ; [(list 'vector-ref (? vector?) (? integer?))
     ; (if (<= 0 v2 (sub1 (vector-length v1)))
         ; (vector-ref v1 v2)
         ; 'err)]
    ; [(list 'make-string (? integer?) (? char?))
     ; (if (<= 0 v1)
         ; (make-string v1 v2)
         ; 'err)]
    ; [(list 'string-ref (? string?) (? integer?))
     ; (if (<= 0 v2 (sub1 (string-length v1)))
         ; (string-ref v1 v2)
         ; 'err)]
    [_ #f]))

(define (optimize-ds ds)
  (match ds
   ['() '()]
   [(cons x xs) 
    (match x
     [(Defn v a e) (cons (Defn v a (optimize-static e)) (optimize-ds xs))])]))

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
     (match (optimize-static e)
       ['err 'err]
       [v (match (optimize-app-args es)
            ['err 'err]
            [vs (cons v vs)])])]))

(define (codepoint? v)
  (and (integer? v)
       (or (<= 0 v 55295)
           (<= 57344 v 1114111))))

