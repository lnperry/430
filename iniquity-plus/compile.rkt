#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" "compile-ops.rkt" a86/ast)

;; Registers used
(define rax 'rax) ; return
(define rbx 'rbx) ; heap
(define rsp 'rsp) ; stack
(define rdi 'rdi) ; arg
(define r8 'r8) ; scratch for arity-check
(define r9 'r9)
(define r10 'r10)
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
    (let ((labelEmpty (gensym 'empty)))
     (seq (Label (symbol->label f))
          ;; r8=num args from caller, xs=num args in definition
          (Cmp r8 (length xs))   
          (Jl 'raise_error_align) ; check arity
          (pop-args (length xs) e x xs)
          ))]
    [(FunCase cs)
     (seq
     ; top level is label for function
       (Label (symbol->label f))
     ; inside here, multiple labels for each function 
       (compile-fun-case f cs))]))

(define (compile-fun-case f cs)
  (let ((labelNext (gensym 'next))) 
    (match cs
     ['() '()]
     [(cons x xs)
      (match x
      [(FunPlain fun-xs fun-e) 
       (seq
        ; if r8 equal to (length fun-xs)
        (Cmp r8 (length fun-xs))
        (Jne labelNext)
        (compile-fun (gensym f) x)
        (Label labelNext)
        (compile-fun-case f xs))]
      [(FunRest rest-xs rest-x e) 
       (seq
         ;if r8 is less (length xs), cont
         (Cmp r8 (length rest-xs))
         (Jl labelNext)
         (compile-fun (gensym f) x)
         (Label labelNext)
         (compile-fun-case f xs))])])))
      
(define (pop-args n e x xs)
  (let ((loopLabel (gensym 'loopPop))
        (condLabel (gensym 'condPop))
        (labelEmpty (gensym 'donePop)))
    (seq 

     ;; just pop as many args as needed, append empty list
     (Sub r8 (length xs)) ; keep r8 so we know how much of rsp to pop
     (Mov r9 r8)
     (compile-value '())
     (Cmp r9 0)
     (Je labelEmpty)
     (Jmp condLabel) ; rax='()
     (Label loopLabel)
     (Mov (Offset rbx 0) rax)
     (Pop rax)
     (Mov (Offset rbx 8) rax)
     (Mov rax rbx)
     (Or rax type-cons)
     (Add rbx 16)
     (Sub r9 1)
     ;check loop condition
     (Label condLabel)
     (Cmp r9 0)
     (Jne loopLabel)
     ; No matter what, I think we want to push rax whether thats a heap ptr or '()
     (Label labelEmpty)
     (Push rax)
     (compile-e e (cons x (reverse xs)))
     ; always + (len xs)+1 
     ; because after push/pop rsp always at (+ (len xs) (len (list x)))
     (Add rsp (* 8 (+ 1 (length xs))))
     (Ret))))

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
  (seq (%%% "Start compile-let")
   (compile-e e1 c)
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
         (compile-es es (cons #f c))
         (Mov r8 (length es))
         (Jmp (symbol->label f))
         (Label r))))

;; Id [Listof Expr] Expr CEnv -> Asm
(define (compile-apply f es e c)
  (let ((r (gensym 'ret)))
    (seq (%%% "Start compile-appply")
     (Lea rax r)
         (Push rax)
         ; rsp ----------v
         ; 0 ... | | | | |Label| ... MAX_RAM
         ; rax = Label
         ; need to update r8 w length of all args together
         ;;; TODO: is r8 messing up in nested calls? is it getting clobbered?
         (Mov r8 (length es))
         ; stack diagram

         (% "Moving length of es into r8")
         (compile-es es (cons #f c))
         ; stack diagram
         ; (define (f x) x) (apply f (cons 1 '()) (cons 1 (cons 2 '())))
         ; rsp --------v
         ; 0 ... | | | |(cons 1 '())|Label| ... MAX_RAM
         ; rax = Label
         (%% "Compile e starting")
         (compile-e e (append (make-list (+ (length es) 1) #f) c)) ; leaves cons ptr in rax
         (%%% "Compile e done")
         ; what gets left in rax after this is run?
         ; does x evaluate to (cons 1 '()) or not?
         ; (run '[(define (append . xss) xss)   (let ((x (cons 1 '()))) (apply append 2 x))])
         ; it is pushing a ptr into rax? why? should maybe step thru w dr racket and see why
         (% "Start of compile-e-list")
         (compile-e-list e c)
         (Jmp (symbol->label f))
         (Label r))))

(define (compile-e-list e c)
  (let ((condLabel (gensym 'condLabelCompileE))
        (loopLabel (gensym 'loopLabelCompileE))
        (emptyLabel (gensym 'emptyLabelCompileE)))
   (match e 
      ; should this be done at runtime?
      ; need special case for empty list '() ?
    [(Empty) (seq)]
    [_       
      (seq 
        ; (cons 1 (cons 2 (cons 3 (cons 4 '()))))
        ; '() -> do no
        ; rbx-------------------------------------------v
        ; 0 ... |'()|4|0bx10010|3|0bx11010|2|0bx100010|1| ... MAX_RAM
        ;  v--------------------------------^
        ; rax : 0bx100010
        ;
        ; (run '[(define (append . xss) xss)    (let ((x '())) (apply append (cons 1 '()) x))])
        ; this breaks. why? this works in racket
        ; need to follow along in Dr. Racket and build out the stack frame as I walk thru debugger
        ; Jose's intuition was I needed to add 1 to my (length es) to account for the label i pushed 
        ; can examine stack in gdb with x/4 $rsp => this shows the top 4 elements of the stack
        ; im seeing some weird artifacts i dont recognize in my stack, where are they coming from?
        ; why is it happening? should probably go line by line in gdb and figure out whats happpening
        ; in my stack
        (Cmp rax 152)
        (Je emptyLabel)
        (Jmp condLabel)
        (Label loopLabel)
        (Xor rax type-cons)
        (Mov r9 (Offset rax 8))
        (Push r9)
        (Add r8 1)
        (Mov rax (Offset rax 0))
        (Label condLabel)
        (Mov r9 rax)
        (Xor r9 type-cons)
        (Mov r9 (Offset r9 0))
        ; TODO: compare rax to '() not 152, but okay while prototyping
        (Cmp r9 152)
        (Jne loopLabel)
        ; (Offset rax 0) == '(), push both 
        (Mov r9 rax)
        (Xor r9 type-cons)
        (Mov r9 (Offset r9 8))
        (Push r9)
        ; only push once, and ignore '() in (Offset rax 0)
        (Add r8 1)
        (Label emptyLabel))])))


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
; (run '[(define (f y . xs) y) (f 5)])
;;(compile (parse '[(define (f b) a) (apply f 5 (cons 6 '()))]))
;(run '[(define (f) 1) (apply f (cons 1 (cons 2 '())))])
