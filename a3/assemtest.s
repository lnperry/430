global entry
        default rel
        section .text
entry:
        mov rax, 3 
        cmp rax, 3
        mov rax, 1
        je nzero519
        mov rax, 3
nzero519:
        ret
