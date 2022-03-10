        global entry
        default rel
        section .text
entry:
        mov rax, 72 
        mov rbx, 3 ; move value of leftClause into rbx 
        cmp rbx, 1 ; cmp rbx with true (did we find a match?)
        je cond1 
        mov rbx, 1 ; move value of leftClause into rbx 
        cmp rbx, 1 ; 
        je cond2
        mov rbx, 3 ; move value of leftClause into rbx 
        cmp rbx, 1
        je cond3
        cmp rbx, 1
        je else1
        mov rax, 82
        ret
cond1:
        mov rax, 14
        mov rbx, 1
        ret

cond2:
        mov rax, 12
        mov rbx, 1
        ret

cond3:
        mov rax, 10
        mov rbx, 1
        ret

else1:
        mov rax, 8
        ret

