global entry
        default rel
        section .text
entry:
        mov rax, 80
        cmp rax, -1
        jg nzero589
        mov rbx, rax
        sub rbx, rax
        sub rbx, rax
        mov rax, rbx
nzero589:
        ret
