#lang racket
(provide optimize optimize-source)
(require "ast.rkt"
         "env.rkt")

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
     (Prog (optimize-ds ds) (optimize-source e))]))

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
    [(Prim1 p e) (optimize-prim1 p (optimize-source e))]
    [(Prim2 p e1 e2) (optimize-prim2 p (optimize-source e1) (optimize-source e2))]
    [(Prim3 p e1 e2 e3) (Prim3 p (optimize-source e1) (optimize-source e2) (optimize-source e3))]
    ; optimize p, if its a literal (int, bool, char) just interp.
    ; or really even only if its just a bool, could go one way or the other
    [(If p e1 e2) (optimize-if (optimize-source p) (optimize-source e1) (optimize-source e2))]
    [(Begin e1 e2) (Begin (optimize-source e1) (optimize-source e2))]
    [(Let x e1 e2) (optimize-let x (optimize-source e1) (optimize-source e2))]
    [(Lam i xs e) (Lam i xs (optimize-source e))]
    [(App e es) (App (optimize-source e) (optimize-app-args es))]
    [(Match e ps es) (Match (optimize-source e) ps (map optimize-source es))]))

(define (optimize-let x e1 e2)
  (if 
   (and (eq? 1 (bindings x e2)) (optimize-let-safe? e2) (optimize-let-safe? e1))
    (optimize-source (replace-x x e1 e2)) 
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
    [(Lam i xs e) (bindings v e)]
    [(App e es) (+ (bindings e) (foldl + 0 (map bindings es)))]
    [(Match e ps es) (+ (bindings e) (foldl + 0 (map optimize-source es)))]))

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
    [(Lam i xs e) (Lam i xs (replace-x var val e))]))
    ; [(App e es) (+ (bindings e) (bindings es))]))
    ; [(Match e ps es) (+ (bindings e) (map optimize-source es))]))

;; Op1 Value -> Answer
(define (optimize-prim1 p1 v)
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
    [(list 'car v1) 
      (match v1 
       [(Prim2 'cons e1 (? optimize-safe?)) (optimize-source e1)]
       [_ (Prim1 p1 v)])]
    [(list 'cdr v1) 
      (match v1 
       [(Prim2 'cons (? optimize-safe?) e2) (optimize-source e2)]
       [_ (Prim1 p1 v)])]
    [_                                                            (Prim1 p1 v)]))

;; Op2 Value Value -> Answer
(define (optimize-prim2 p v1 v2)
  (match (list p v1 v2)
    [(list '+ (Int (? integer? v1)) (Int (? integer? v2)))  (Int (+ v1 v2))]
    [(list '- (Int (? integer? v1)) (Int (? integer? v2)))  (Int (- v1 v2))]
    [(list '< (Int (? integer? v1)) (Int (? integer? v2)))  (Bool (< v1 v2))]
    [(list '= (Int (? integer? v1)) (Int (? integer? v2)))  (Bool (= v1 v2))]
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
    [(Lam i xs e) (optimize-safe? e)]
    [(App e es) (and (optimize-safe? e) (andmap optimize-safe? es))]
    [(Match e ps es) (and (optimize-safe? e) (map optimize-safe? es))]))

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
    [(Lam i xs e) (optimize-let-safe? e)]
    [(App e es) (and (optimize-let-safe? e) (andmap optimize-let-safe? es))]
    [(Match e ps es) (and (optimize-let-safe? e) (map optimize-let-safe? es))]))

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
    [_                                                           #f]))



(define (prim2-safe p v1 v2)
  (match (list p v1 v2)
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
    [_ (Prim2 p v1 v2)]))
 
; (define (optimize-safe? e)
  ; (match e
    ; [#t #t]
    ; [#f #f]
    ; [(Int i)  #t]
    ; [(Bool b) #t]
    ; [(Char c) #t]
    ; [(Eof)    #f] ;i/o unsafe
    ; [(Empty)  #t]
    ; [(Var x)  #f] ;var reference unsafe
    ; [(Str s)  #t]
    ; [(Prim0 'void) #f] ;i/o unsafe
    ; [(Prim0 'read-byte) #f] ;i/o unsafe
    ; [(Prim0 'peek-byte) #f] ;i/o unsafe
    ; [(Prim1 p e) (prim1-safe p (optimize-source e) acc)]
    ; [(Prim2 p e1 e2) (prim2-safe p (optimize-safe e1) (optimize-safe e2))]
    ; [(Prim3 p e1 e2 e3) (Prim3 p (optimize-source e1) (optimize-source e2) (optimize-source e3))]
    ; [(If p e1 e2) (optimize-if (optimize-source p) (optimize-source e1) (optimize-source e2))]
    ; [(Begin e1 e2) (Begin (optimize-source e1) (optimize-source e2))]
    ; [(Let x e1 e2) (Let x (optimize-source e1) (optimize-source e2))]
    ; [(Lam i xs e) (Lam i xs (optimize-source e))]
    ; [(App e es) (App (optimize-source e) (optimize-app-args es))]
    ; [(Match e ps es) (Match (optimize-source e) ps (map optimize-source es))]))


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
(define (optimize-app-args es)
  (match es
    ['() '()]
    [(cons e es)
     (match (optimize-source e)
       ['err 'err]
       [v (match (optimize-app-args es)
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

(define (codepoint? v)
  (and (integer? v)
       (or (<= 0 v 55295)
           (<= 57344 v 1114111))))

