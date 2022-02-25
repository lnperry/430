global entry
        default rel
        section .text
entry:
        mov rax, -42 
        mov rbx, 0 
        sub rbx, rax
        mov rax, rbx
        ret
