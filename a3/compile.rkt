#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" a86/ast)

;; Expr -> Asm
(define (compile e)
  (prog (Label 'entry)
        (compile-e e)
        (Ret)))

;; Expr -> Asm
(define (compile-e e)
  (match e
    [(Int i)           (compile-integer i)]
    [(Bool b)          (compile-boolean b)]
    [(Prim1 p e)       (compile-prim p e)]
    [(If e1 e2 e3)     (compile-if e1 e2 e3)]
    ;; TODO: Handle cond
    [(Cond e1 e2)      (compile-cond e1 e2)]
    ;; TODO: Handle case
    ))


;; Integer -> Asm
(define (compile-integer i)
  (seq (Mov 'rax (value->bits i))))

;; Boolean -> Asm
(define (compile-boolean b)
  (seq (Mov 'rax (value->bits b))))

;; Op Expr -> Asm
(define (compile-prim p e)
  (seq (compile-e e)
       (match p
         ['add1 (Add 'rax (value->bits 1))]
         ['sub1 (Sub 'rax (value->bits 1))]
         ;; TODO: Handle abs, -, and not
         ['zero?
          (let ((l1 (gensym 'nzero)))
            (seq (Cmp 'rax 0)
                 (Mov 'rax val-true)
                 (Je l1)
                 (Mov 'rax val-false)
                 (Label l1)))] 
         ['abs
          (let ((l1 (gensym 'nabs)))
            (seq (Cmp 'rax 0)
                 (Jg l1)
                 (Mov 'rbx 'rax)
                 (Sub 'rbx 'rax)
                 (Sub 'rbx 'rax)
                 (Mov 'rax 'rbx)
                 (Label l1)))] 
         ['-
          (let ((l1 (gensym 'n-)))
            (seq (Mov 'rbx 0)
                 (Sub 'rbx 'rax)
                 (Mov 'rax 'rbx)))]
         ['not
          (let ((l1 (gensym 'nnot)))
            (seq (Cmp 'rax val-false)
                 (Mov 'rax val-true)
                 (Je l1)
                 (Mov 'rax val-false)
                 (Label l1)))]))) 

;; Expr Expr Expr -> Asm
(define (compile-if e1 e2 e3)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1)
         (Cmp 'rax val-false)
         (Je l1)
         (compile-e e2)
         (Jmp l2)
         (Label l1)
         (compile-e e3)
         (Label l2))))


(define (compile-cond e1 e2)
    (match e1
      ['() (seq
             (%% "Cond: Else?")
             (compile-e e2))]
  
      ;; if its a list of clauses recurse
      [(cons x xs) 
       (match x 
         [(Clause ex1 ex2) (seq 
                             (%% "Cond: Compile ex1 of clause?")
                             (compile-e ex1)
                             (%% "Cond: Compile ex2 of clause")
                             (compile-e ex2)
                             (compile-cond xs e2))])]))



