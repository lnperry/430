#lang racket
(require "ast.rkt")
(provide optimize-prim1 optimize-prim2 optimize-prim3)

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
    [_                                                            (Prim1 p1 v)]))

;; Op2 Value Value -> Answer
(define (optimize-prim2 p v1 v2)
  (match (list p v1 v2)
    [(list '+ (Int (? integer? v1)) (Int (? integer? v2)))  (Int (+ v1 v2))]
    [(list '- (Int (? integer? v1)) (Int (? integer? v2)))  (Int (- v1 v2))]
    [(list '< (Int (? integer? v1)) (Int (? integer? v2)))  (Bool (< v1 v2))]
    [(list '= (Int (? integer? v1)) (Int (? integer? v2)))  (Bool (= v1 v2))]
    ; [(list 'cons v1 v2)                   (cons v1 v2)]
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

;; Op3 Value Value Value -> Answer
(define (optimize-prim3 p v1 v2 v3)
  (match (list p v1 v2 v3)
    [(list 'vector-set! (? vector?) (? integer?) _)
     (if (<= 0 v2 (sub1 (vector-length v1)))
         (vector-set! v1 v2 v3)
         'err)]
    [_ 'err]))

;; Any -> Boolean
(define (codepoint? v)
  (and (integer? v)
       (or (<= 0 v 55295)
           (<= 57344 v 1114111))))
