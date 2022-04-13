        global entry
        default rel
        section .text
        extern peek_byte
        extern read_byte
        extern write_byte
        extern raise_error
        global entry
entry:
        mov rbx, rdi
        mov rax, 152
        push rax
        lea rax, [rel ret667]
        push rax
        mov r8, 1               ; Moving length of es into r8
        mov rax, 32
        push rax
        ;; Compile e starting
        mov rax, [rsp + 0]
;;; Compile e done
                                ; Start of compile-e-list
        cmp rax, 152
        je emptyLabelCompileE670
        jmp condLabelCompileE668
loopLabelCompileE669:
        xor rax, 2
        mov r9, [rax + 8]
        push r9
        add r8, 1
        mov rax, [rax + 0]
condLabelCompileE668:
        mov r9, rax
        xor r9, 2
        mov r9, [r9 + 0]
        cmp r9, 152
        jne loopLabelCompileE669
        mov r9, rax
        xor r9, 2
        mov r9, [r9 + 8]
        push r9
        add r8, 1
emptyLabelCompileE670:
        jmp label_append_bd8f7e2d3b0458
ret667:
        add rsp, 8
        ret
label_append_bd8f7e2d3b0458:
        cmp r8, 0
        jl raise_error_align
        sub r8, 0
        mov r9, r8
        mov rax, 152
        cmp r9, 0
        je donePop674
        jmp condPop673
loopPop672:
        mov [rbx + 0], rax
        pop rax
        mov [rbx + 8], rax
        mov rax, rbx
        or rax, 2
        add rbx, 16
        sub r9, 1
condPop673:
        cmp r9, 0
        jne loopPop672
donePop674:
        push rax
        mov rax, [rsp + 0]
        add rsp, 8
        ret
raise_error_align:
        or rsp, 8
        jmp raise_error

