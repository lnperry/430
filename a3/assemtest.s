;    #s(Cond
;        (
;           #s(Clause #s(Bool #f) #s(Int 13)) 
;           #s(Clause #s(Bool #t) #s(Int 12))
;           #s(Clause #s(Bool #t) #s(Int 11))
;        )
;      #s(Int 10))
        global entry
        default rel
        section .text
entry:
        ; 1. basically, compile (eval) left hand side of brackets
        ; which will store the value is rax
        ; 2. if its true, put right hand side in rax. jump to DONE 
        ; 3. if more clauses, repeat step 1 and continue parsing clauses
        ; 4. if no more clauses, stick value of else in rax. DONE 

        ; keep "truth" of last computed clause in some register
        ; if that register is "true" (compare to true, then JEQ) 
        ; then jump to done
        ; if reg !"true", compile clause stick "truth" in that same register
        
        ; if out of clauses, do else
        mov rbx, 3 ; move value of leftClause into rbx 
        cmp rbx, 1 ; cmp rbx with true (did we find a match?)
        je cond1 
        cmp rbx, 1 ; 
        je cond2
        cmp rbx, 1
        je cond3
        cmp rbx, 1
        je else
        ret

cond1:
        mov rax, 13
        mov rbx, 1
        jmp done

cond2:
        mov rax, 12
        mov rbx, 1
        jmp done

cond3:
        mov rax, 11
        mov rbx, 1
        jmp done

else1:
        move rax, 10
