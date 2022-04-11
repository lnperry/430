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
        lea rax, [rel ret600]
        push rax
        mov rax, 16
        push rax
        mov rax, 32
        push rax
        mov rax, 48
        push rax
        mov rax, 152
        mov [rbx + 0], rax
        pop rax
        mov [rbx + 8], rax
        mov rax, rbx
        or rax, 2
        add rbx, 16
        mov [rbx + 0], rax
        pop rax
        mov [rbx + 8], rax
        mov rax, rbx
        or rax, 2
        add rbx, 16
        mov [rbx + 0], rax
        pop rax
        mov [rbx + 8], rax
        mov rax, rbx
        or rax, 2
        add rbx, 16
        push rax
        mov r8, 1
        jmp label_append_bd8f7e2d3b0458
ret600:
        ret
label_f_f5e238a9d16:
        cmp r8, 0
        jl raise_error_align
        sub r8, 0
        mov r9, r8
        mov rax, 152
        cmp r9, 0
        je donePop604
        jmp condPop603
loopPop602:
        mov [rbx + 0], rax
        pop rax
        mov [rbx + 8], rax
        mov rax, rbx
        or rax, 2
        add rbx, 16
        sub r9, 1
condPop603:
        cmp r9, 0
        jne loopPop602
donePop604:
        push rax
        mov rax, [rsp + 0]
        add rsp, 8
        ret
label_append_bd8f7e2d3b0458:
        cmp r8, 0
        jl raise_error_align
        sub r8, 0
        mov r9, r8
        mov rax, 152
        cmp r9, 0
        je donePop608
        jmp condPop607
loopPop606:
        mov [rbx + 0], rax
        pop rax
        mov [rbx + 8], rax
        mov rax, rbx
        or rax, 2
        add rbx, 16
        sub r9, 1
condPop607:
        cmp r9, 0
        jne loopPop606
donePop608:
        push rax
        mov rax, [rsp + 0]      ; eq-imm?
        cmp rax, 152
        mov rax, 24
        je g611
        mov rax, 56
g611:
        cmp rax, 56
        je if609
        mov rax, 152
        jmp if610
if609:
        lea rax, [rel ret612]
        push rax                ; Moving length of es into r8
        mov r8, 1
        mov rax, [rsp + 8]
        mov r9, rax
        and r9, 7
        cmp r9, 2
        jne raise_error_align
        xor rax, 2
        mov rax, [rax + 0]
        push rax ; this is where we are
        mov rax, [rsp + 8]
        mov r9, rax
        and r9, 7
        cmp r9, 2
        ; breaking here according to gdb, why is rax an int not a cons?
        ; why is the thing on the stack not an int?
        jne raise_error_align
        xor rax, 2
        mov rax, [rax + 0]      ; Start of compile-e-list
        mov r9, rax
        mov rax, 152
        cmp r9, rax
        mov rax, r9
        je emptyLabelCompileE615
        jmp condLabelCompileE613
loopLabelCompileE614:
        xor rax, 2
        mov r9, [rax + 8]
        push r9
        add r8, 1
        mov rax, [rax + 0]
condLabelCompileE613:
        mov r9, rax
        and r9, 7
        cmp r9, 2
        jne raise_error_align
        mov r9, rax
        xor r9, 2
        mov r9, [r9 + 0]
        and r9, 7
        cmp r9, 2
        je loopLabelCompileE614
        xor rax, 2
        mov r9, [rax + 8]
        push r9
        add r8, 1
emptyLabelCompileE615:
        jmp label_append_bd8f7e2d3b0458
ret612:
if610:
        add rsp, 8
        ret
label_list_80111a34bb785d:
        cmp r8, 0
        jl raise_error_align
        sub r8, 0
        mov r9, r8
        mov rax, 152
        cmp r9, 0
        je donePop619
        jmp condPop618
loopPop617:
        mov [rbx + 0], rax
        pop rax
        mov [rbx + 8], rax
        mov rax, rbx
        or rax, 2
        add rbx, 16
        sub r9, 1
condPop618:
        cmp r9, 0
        jne loopPop617
donePop619:
        push rax
        mov rax, [rsp + 0]
        add rsp, 8
        ret
label_flatten_1ccb3ac48c0d35:
        cmp r8, 1
        jne raise_error_align
        lea rax, [rel ret620]
        push rax                ; Moving length of es into r8
        mov r8, 0
        mov rax, [rsp + 8]      ; Start of compile-e-list
        mov r9, rax
        mov rax, 152
        cmp r9, rax
        mov rax, r9
        je emptyLabelCompileE623
        jmp condLabelCompileE621
loopLabelCompileE622:
        xor rax, 2
        mov r9, [rax + 8]
        push r9
        add r8, 1
        mov rax, [rax + 0]
condLabelCompileE621:
        mov r9, rax
        and r9, 7
        cmp r9, 2
        jne raise_error_align
        mov r9, rax
        xor r9, 2
        mov r9, [r9 + 0]
        and r9, 7
        cmp r9, 2
        je loopLabelCompileE622
        xor rax, 2
        mov r9, [rax + 8]
        push r9
        add r8, 1
emptyLabelCompileE623:
        jmp label_append_bd8f7e2d3b0458
ret620:
        add rsp, 8
        ret
raise_error_align:
        or rsp, 8
        jmp raise_error

