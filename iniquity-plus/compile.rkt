#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" "compile-ops.rkt" a86/ast)

;; Registers used
(define rax 'rax) ; return
(define rbx 'rbx) ; heap
(define rsp 'rsp) ; stack
(define rdi 'rdi) ; arg
(define r8 'r8) ; scratch for arity-check
(define r9 'r9) ; scratch for pop loop
(define r10 'r10) ; scratch for pop loop

;; type CEnv = [Listof Variable]

;; Prog -> Asm
(define (compile p)
  (match p
    [(Prog ds e)
     (prog (externs)
           (Global 'entry)
           (Label 'entry)
           (Mov rbx rdi) ; recv heap pointer
           (compile-e e '())
           (Ret)
           (compile-defines ds)
           (Label 'raise_error_align)
           (Or rsp 8)
           (Jmp 'raise_error))]))

(define (externs)
  (seq (Extern 'peek_byte)
       (Extern 'read_byte)
       (Extern 'write_byte)
       (Extern 'raise_error)))

;; [Listof Defn] -> Asm
(define (compile-defines ds)
  (match ds
    ['() (seq)]
    [(cons d ds)
     (seq (compile-define d)
          (compile-defines ds))]))

;; Defn -> Asm
(define (compile-define d)
  (match d
    [(Defn f fun)
     (compile-fun f fun)]))

;; Id Fun -> Asm
(define (compile-fun f fun)
  (match fun
    [(FunPlain xs e)
     (seq (Label (symbol->label f))
          (Cmp r8 (length xs)) ; check arity
          (Jne 'raise_error_align)
          (compile-e e (reverse xs))
          (Add rsp (* 8 (length xs)))
          (Ret))]
    ;; TODO: handle other kinds of functions
    [(FunRest xs x e) 
     (seq (Label (symbol->label f))
          (Cmp r8 (length xs)) ; check arity
          (Jl 'raise_error_align)
          ;; push empty list to stack
          ;; TODO: replace with n-m 
          ;;       args pushed to stack
          ;(compile-e (Empty) '())
          ;(Push rax)
          ;; TODO: handle empty list as spec case?
          (pop-args (length xs) e x xs)
          ;;(compile-e e (cons x (reverse xs)))
          ;; add 1 because empty list is now in stack
          ;;(Add rsp (* 8 (length xs)))
          (Ret))]))
;
; rsp------------------v
;  |  |'() |3 |  |  |3|2|1|ret
; rbx----------^
; rax=&rbx
; r9=1
; r8=2
;
;
;
(define (pop-args n e x xs)
  (let ((loopLabel (gensym 'loop))
        (condLabel (gensym 'cond)))
    (seq (Mov r9 r8)
         (Sub r9 n)
         (Mov r8 r9) ; save how many args we popped
         (compile-e (Empty) '()) ; rax='()
         (Jmp condLabel)
         (Label loopLabel)
         ;; need to tag second thing in cons cell as cons cell
         (Mov (Offset rbx 0) rax)
         (Pop rax)
         (Mov (Offset rbx 8) rax)
         (Mov rax rbx)
         (Or rax type-cons)
         (Add rbx 16)
         (Sub r9 1)
         ;; check loop condition
         (Label condLabel)
         (Cmp r9 0)
         (Jne loopLabel)
         ;; Push rax list back onto stack
         (Push rax)
         ;; invoke function
         (compile-e e (cons x (reverse xs)))
         ;; pop rest of stack args off
         (Sal r8 3)
         (Add rsp r8))))
         ;; cons from prim

;; Expr CEnv -> Asm
(define (compile-e e c)
  (match e
    [(Int i)            (compile-value i)]
    [(Bool b)           (compile-value b)]
    [(Char c)           (compile-value c)]
    [(Eof)              (compile-value eof)]
    [(Empty)            (compile-value '())]
    [(Var x)            (compile-variable x c)]
    [(Str s)            (compile-string s)]
    [(Prim0 p)          (compile-prim0 p c)]
    [(Prim1 p e)        (compile-prim1 p e c)]
    [(Prim2 p e1 e2)    (compile-prim2 p e1 e2 c)]
    [(Prim3 p e1 e2 e3) (compile-prim3 p e1 e2 e3 c)]
    [(If e1 e2 e3)      (compile-if e1 e2 e3 c)]
    [(Begin e1 e2)      (compile-begin e1 e2 c)]
    [(Let x e1 e2)      (compile-let x e1 e2 c)]
    [(App f es)         (compile-app f es c)]
    [(Apply f es e)     (compile-apply f es e c)]))

;; Value -> Asm
(define (compile-value v)
  (seq (Mov rax (imm->bits v))))

;; Id CEnv -> Asm
(define (compile-variable x c)
  (let ((i (lookup x c)))
    (seq (Mov rax (Offset rsp i)))))

;; String -> Asm
(define (compile-string s)
  (let ((len (string-length s)))
    (if (zero? len)
        (seq (Mov rax type-str))
        (seq (Mov rax len)
             (Mov (Offset rbx 0) rax)
             (compile-string-chars (string->list s) 8)
             (Mov rax rbx)
             (Or rax type-str)
             (Add rbx
                  (+ 8 (* 4 (if (odd? len) (add1 len) len))))))))

;; [Listof Char] Integer -> Asm
(define (compile-string-chars cs i)
  (match cs
    ['() (seq)]
    [(cons c cs)
     (seq (Mov rax (char->integer c))
          (Mov (Offset rbx i) 'eax)
          (compile-string-chars cs (+ 4 i)))]))

;; Op0 CEnv -> Asm
(define (compile-prim0 p c)
  (compile-op0 p))

;; Op1 Expr CEnv -> Asm
(define (compile-prim1 p e c)
  (seq (compile-e e c)
       (compile-op1 p)))

;; Op2 Expr Expr CEnv -> Asm
(define (compile-prim2 p e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons #f c))
       (compile-op2 p)))

;; Op3 Expr Expr Expr CEnv -> Asm
(define (compile-prim3 p e1 e2 e3 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons #f c))
       (Push rax)
       (compile-e e3 (cons #f (cons #f c)))
       (compile-op3 p)))

;; Expr Expr Expr CEnv -> Asm
(define (compile-if e1 e2 e3 c)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1 c)
         (Cmp rax val-false)
         (Je l1)
         (compile-e e2 c)
         (Jmp l2)
         (Label l1)
         (compile-e e3 c)
         (Label l2))))

;; Expr Expr CEnv -> Asm
(define (compile-begin e1 e2 c)
  (seq (compile-e e1 c)
       (compile-e e2 c)))

;; Id Expr Expr CEnv -> Asm
(define (compile-let x e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons x c))
       (Add rsp 8)))

;; Id [Listof Expr] CEnv -> Asm
;; The return address is placed above the arguments, so callee pops
;; arguments and return address is next frame
(define (compile-app f es c)
 ;; pop 
  (let ((r (gensym 'ret)))
    (seq (Lea rax r)
         (Push rax)
         ;; push empty list to stack
         (compile-es es (cons #f c))
         ;; TODO: communicate argument count to called function
         ;; push empty list
         ;(compile-value '()) ; put empty list in rax
         ;(Push rax) ; push rax
         (Mov r8 (length es))
         (Jmp (symbol->label f))
         (Label r))))

;; Id [Listof Expr] Expr CEnv -> Asm
(define (compile-apply f es e c)
  ;; TODO: implement apply
  (seq))

;; [Listof Expr] CEnv -> Asm
(define (compile-es es c)
  (match es
    ['() '()]
    [(cons e es)
     (seq (compile-e e c)
          (Push rax)
          (compile-es es (cons #f c)))]))

;; Id CEnv -> Integer
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y rest)
     (match (eq? x y)
       [#t 0]
       [#f (+ 8 (lookup x rest))])]))

;; Symbol -> Label
;; Produce a symbol that is a valid Nasm label
(define (symbol->label s)
  (string->symbol
   (string-append
    "label_"
    (list->string
     (map (λ (c)
            (if (or (char<=? #\a c #\z)
                    (char<=? #\A c #\Z)
                    (char<=? #\0 c #\9)
                    (memq c '(#\_ #\$ #\# #\@ #\~ #\. #\?)))
                c
                #\_))
         (string->list (symbol->string s))))
    "_"
    (number->string (eq-hash-code s) 16))))

; (begin (require "parse.rkt" "interp.rkt" "types.rkt" a86/interp
                ; rackunit a86/printer "ast.rkt" "heap.rkt" "heap.rkt"
                ; "env.rkt"
                ; "interp-prims-heap.rkt"
                ; "ast.rkt"
                ; "unload-bits-asm.rkt") (current-objs '("runtime.o")) (define
                                                                       ; (run p)
                                                                       ; (unload/free (asm-interp (compile (parse p))))))
;(run '[(define (f y . xs) y) (f 5)])
