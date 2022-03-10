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
       mov rax, 10
       cmp rax, 20
       je label1
       mov rax, 20
       ret
label1:
       mov rax, 50

