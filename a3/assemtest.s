;;; cond '(cond [(not #t) 3] [7 4] [else 5])

;;; [(not #t) 4] [7 4] [else 5]

;;; '#s(Cond
;;;     (#s(Clause #s(Prim1 not #s(Bool #t)) #s(Int 3))
;;;      #s(Clause #s(Int 7) #s(Int 4)))
;;;     #s(Int 5))


        global entry
        default rel
        section .text
entry:
        mov rax, 1 ;input 1==true, 3==false 
        ; deal with bools in a sec

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
  
        mov rax, 
        cmp rax, 1 ;true 
        ret

else:
       move rax, 5


