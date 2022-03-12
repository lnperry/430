#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" a86/ast)

(define rax 'rax) ; return
(define rdi 'rdi) ; arg
(define r8  'r8)  ; scratch in +, -
(define r9  'r9)  ; scratch in assert-type
(define rsp 'rsp) ; stack
(define r10 'r10) ; scratch in arithmetic-shift

;; Op0 CEnv -> Asm
(define (compile-op0 p c)
  (match p
    ['void      (seq (Mov rax val-void))]
    ['read-byte (seq (pad-stack c)
                     (Call 'read_byte)
                     (unpad-stack c))]
    ['peek-byte (seq (pad-stack c)
                     (Call 'peek_byte)
                     (unpad-stack c))]))

;; Op1 CEnv -> Asm
(define (compile-op1 p c)
  (match p
    ['add1
     (seq (assert-integer rax c)
          (Add rax (value->bits 1)))]
    ['sub1
     (seq (assert-integer rax c)
          (Sub rax (value->bits 1)))]
    ['zero?
     (let ((l1 (gensym)))
       (seq (assert-integer rax c)
            (Cmp rax 0)
            (Mov rax val-true)
            (Je l1)
            (Mov rax val-false)
            (Label l1)))]
    ['char?
     (let ((l1 (gensym)))
       (seq (And rax mask-char)
            (Xor rax type-char)
            (Cmp rax 0)
            (Mov rax val-true)
            (Je l1)
            (Mov rax val-false)
            (Label l1)))]
    ['char->integer
     (seq (assert-char rax c)
          (Sar rax char-shift)
          (Sal rax int-shift))]
    ['integer->char
     (seq (assert-codepoint c)
          (Sar rax int-shift)
          (Sal rax char-shift)
          (Xor rax type-char))]
    ['eof-object? (eq-imm val-eof)]
    ['write-byte
     (seq (assert-byte c)
          (pad-stack c)
          (Mov rdi rax)
          (Call 'write_byte)
          (unpad-stack c)
          (Mov rax val-void))]))

;; Op2 CEnv -> Asm
(define (compile-op2 p c)
  (match p
    ['+
     (seq (Pop r8)
          (assert-integer r8 c)
          (assert-integer rax c)
          (Add rax r8))]
    ['-
     (seq (Pop r8)
          (assert-integer r8 c)
          (assert-integer rax c)
          (Sub r8 rax)
          (Mov rax r8))]
    ['arithmetic-shift 
      (let ((l1 (gensym 'if))
           (l2 (gensym 'if)))
        (seq 
          (Cmp rax 0)
          (Jl l1)
          ;; if rax >= 0, this execs
          (compile-left-shift c)
          (Jmp l2) ;; jump to end
          (Label l1)
          ;; if rax < 0, this execs
          (compile-right-shift c)
          (Label l2)))]))
      ;; (if rax<0 compile-left-shift compile-right-shift)
      ;; i dont think compile if works bc i need to compile certain code
      ;; based on what a value is and i dont know when im compiling
      ;; what is actually in that register
      ;; so ill need to jump to diff labels depending
      ;; so prob want to steal if stmt code cmp rax to 0 then jump accordingly
(define (compile-left-shift c)
  (let ((loopLabel (gensym 'loop))
        (condLabel (gensym 'cond)))
    (seq 
         (Pop r8)
         (assert-integer r8 c)
         (assert-integer rax c)
         ;; shift from our compiler representation to what programmer means
         (Sar rax 1)
         (Jmp condLabel)
         (Label loopLabel)
         (Sal r8 1) 
         (Sub rax 1)
         (Label condLabel)
         (Cmp rax 0) 
         (Jne loopLabel)
         (Mov rax r8)
         ;; need to tag rax with int!(maybe)
         )))

(define (compile-right-shift c)
  (let ((loopLabel (gensym 'loop))
        (condLabel (gensym 'cond)))
    (seq 
         (Pop r8)
         (assert-integer r8 c)
         (assert-integer rax c)
         ;; convert to positive representation 
         ;;(Mov r10 rax) ;;r10 = -6 (really -12 in bits)
         ;;(Sub r10 rax)
         ;;(Sub r10 rax)
         ;;(Add rax r10)
         ;;(Add rax r10)
         ;;(Sar r10 1) ;;r10 = -3 (really -6 in bits) 
         ;;(Sub r10 rax)
         ;;(Mov rax r10)
         ;; shift from our compiler representation to what programmer means
         (Sar rax 1)
         (Jmp condLabel)
         (Label loopLabel)
         (Sar r8 1) 
         (Add rax 1)
         (Label condLabel)
         (Cmp rax 0) 
         (Jne loopLabel)
         (Mov rax r8)
         (Sub rax 1)
         )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (assert-type mask type)
  (λ (arg c)
    (seq (Mov r9 arg)
         (And r9 mask)
         (Cmp r9 type)
         (Jne (error-label c)))))

(define (type-pred mask type)
  (let ((l (gensym)))
    (seq (And rax mask)
         (Cmp rax type)
         (Mov rax (value->bits #t))
         (Je l)
         (Mov rax (value->bits #f))
         (Label l))))

(define assert-integer
  (assert-type mask-int type-int))
(define assert-char
  (assert-type mask-char type-char))

(define (assert-codepoint c)
  (let ((ok (gensym)))
    (seq (assert-integer rax c)
         (Cmp rax (value->bits 0))
         (Jl (error-label c))
         (Cmp rax (value->bits 1114111))
         (Jg (error-label c))
         (Cmp rax (value->bits 55295))
         (Jl ok)
         (Cmp rax (value->bits 57344))
         (Jg ok)
         (Jmp (error-label c))
         (Label ok))))

(define (assert-byte c)
  (seq (assert-integer rax c)
       (Cmp rax (value->bits 0))
       (Jl (error-label c))
       (Cmp rax (value->bits 255))
       (Jg (error-label c))))

;; Imm -> Asm
(define (eq-imm imm)
  (let ((l1 (gensym)))
    (seq (Cmp rax imm)
         (Mov rax val-true)
         (Je l1)
         (Mov rax val-false)
         (Label l1))))

;; CEnv -> Asm
;; Pad the stack to be aligned for a call
(define (pad-stack c)
  (match (even? (length c))
    [#t (seq (Sub rsp 8))]
    [#f (seq)]))

;; CEnv -> Asm
;; Undo the stack alignment after a call
(define (unpad-stack c)
  (match (even? (length c))
    [#t (seq (Add rsp 8))]
    [#f (seq)]))

;; CEnv -> Label
;; Determine correct error handler label to jump to.
(define (error-label c)
  (match (even? (length c))
    [#t 'raise_error]
    [#f 'raise_error_align]))
