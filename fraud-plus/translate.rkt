#lang racket
(provide translate)
(require "ast.rkt")

;; S-Expr -> Expr
(define (translate s)
  (match s
    [(? integer?)              (Int s)]
    [(? boolean? s)            (Bool s)]
    [(? char? s)               (Char s)]
    ['eof                      (Eof)]
    [(? symbol? s)             (Var s)]
    [(list 'not e)             (If (translate e) (Bool #f) (Bool #t))]    
    [(list (? op0? o))         (Prim0 o)]
    [(list (? op1? o) e)       (Prim1 o (translate e))]
    [(list (? op2? o) e1 e2)   (Prim2 o (translate e1) (translate e2))]
    [(cons (? opN? o) es)      (translate-primn o es)]
    [(list 'begin e1 e2)       (Begin (translate e1) (translate e2))]      
    [(list 'if e1 e2 e3)       (If (translate e1) (translate e2) (translate e3))]
    [(list 'let  bs e)         (translate-let  bs e)]
    [(list 'let* bs e)         (translate-let* bs e)]
    [(cons 'cond cs)           (translate-cond cs)]
    [(cons 'case (cons ev cs)) (translate-case ev cs)]
    [_                         (error "translate error" s)]))

;; Any -> Boolean
(define (op0? x)
  (memq x '(read-byte peek-byte)))
(define (op1? x)
  (memq x '(add1 sub1 zero? char? integer->char char->integer
                 write-byte eof-object?
                 - abs integer? boolean?)))
(define (op2? x)
  (memq x '(-)))
(define (opN? x)
  (memq x '(+)))

;; S-Expr S-Expr -> Let
(define (translate-primn o es)
  (match o
    ['+ 
     (match es
       [(list x y) (Prim2 o (translate x) (translate y))]
       [(cons x xs) (Prim2 o (translate x) (translate-primn o xs))]
       ['() '()])]
    [_ 'err]))

;; S-Expr S-Expr -> Let
(define (translate-let bs e)
  (match bs
    ['() (Let '() '() (translate e))]
    [(cons (list (? symbol? x1) e1) bs)
     (match (translate-let bs e)
       [(Let xs es e)
        (Let (cons x1 xs) (cons (translate e1) es) e)])]
    [else (error "translate error")]))

;; S-Expr S-Expr -> Let
(define (translate-let* bs e)
  (match bs
    ['() (Let* '() '() (translate e))]
    [(cons (list (? symbol? x1) e1) bs)
     (match (translate-let* bs e)
       [(Let* xs es e)
        (Let* (cons x1 xs) (cons (translate e1) es) e)])]
    [else (error "translate error")]))

;; S-Expr -> Cond
(define (translate-cond cs)
  (match cs
    [(list (list 'else e)) (Cond '() (translate e))]
    [(cons (list p e) css)
     (match (translate-cond css)
       [(Cond cs el)
        (Cond (cons (Clause (translate p) (translate e)) cs) el)])]
    [_ (error "translate error")]))    

;; S-Expr S-Expr -> Case
(define (translate-case ev cs)
  (match cs
    [(list (list 'else e)) (Case (translate ev) '() (translate e))]
    [(cons (list ds e) css)
     (match (translate-case ev css)
       [(Case ev cs el)
        (Case ev (cons (Clause (translate-datums ds) (translate e)) cs) el)])]
    [_ (error "translate error")]))

;; S-Expr -> [Listof Datum]
(define (translate-datums ds)
  (match ds
    ['() '()]
    [(cons (? integer? i) ds)
     (cons i (translate-datums ds))]
    [(cons (? boolean? b) ds)
     (cons b (translate-datums ds))]
    [(cons (? char? c) ds)
     (cons c (translate-datums ds))]
    [(cons 'eof ds)
     (cons eof (translate-datums ds))]
    [_ (error "translate error")]))
