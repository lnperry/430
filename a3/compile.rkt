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
    [(Int i)             (compile-integer i)]
    [(Bool b)            (compile-boolean b)]
    [(Prim1 p e)         (compile-prim p e)]
    [(If e1 e2 e3)       (compile-if e1 e2 e3)]
    ;; TODO: Handle cond
    [(Cond cls els)      (compile-cond cls els)]
    ;; TODO: Handle case
    [(Case e cls els)    (Mov 'rax 10)]
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

;; (parse '(cond [(zero? -1) 1] [(if 100 200 300) 2] [else 3]))
;; '#s(Cond
;;    (#s(Clause #s(Prim1 zero? #s(Int -1)) #s(Int 1))
;;     #s(Clause #s(If #s(Int 100) #s(Int 200) #s(Int 300)) #s(Int 2)))
;;    #s(Int 3))

;; the first time i run compile-cond i need to set the rbx flag
;; i think the way to do this is with a helper
;; call the helper from up top
;; then in the helper call this

(define (compile-cond e1 e2)
  ;; put false in rbx as flag for subsequent assembely
  (let ((l1 (gensym 'endCond)))
    (seq
      (%% "C: Put false in rbx")
      (Mov 'rbx val-false)
      (compile-cond-helper e1 e2 l1)
      (Label l1))))




;;(compile-if e1 e2 e3)

;;(compile-if lCl1 rCl1 (compile-if lCl2 rCl2 els))

(define (compile-cond cls els)
  (match cls
    [(cons x xs) (match x
			[(Clause lCl rCl) (compile-if lCl rCl (compile-cond xs els))])]
    ['() (compile-e els)]))


(case 3 [(1 2 4) 1] [(1 2 4) 9] [else 5])

(if (equal 3 1) 1 (if (equal 2 3) 1 (...

(if (iterate-check 3 (1 2 4)) 1 (if (iterate-check (1 2 4) 9) 5))

(define (compile-iterate-check e l)
  (match l
	 [(cons x xs) (compile-if (compile-equal e x) (Bool #t) (compile-iterate-check e x))]
	 ['() (Bool #f)]))

(define (compile-equal e x)
  (let ((l1 (gensym 'equal?)))
  (seq
    (compile-e e)
    (Mov 'rbx 'rax)
    (compile-e x)
    (Cmp 'rbx 'rax)
    (Mov 'rax val-true)
    (Je l1)
    (Mov 'rax val-false)
    (Label l1))))
