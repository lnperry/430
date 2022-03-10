global entry
        default rel
        section .text
entry:
        ;; C: Put false in rbx
        mov rbx, 3
        ;; C: Value of lClause
        mov rax, 2
        cmp rax, 0
        mov rax, 1
        je nzero560
        mov rax, 3
nzero560:
        ;; C: Is rbx truthy?
        cmp rbx, 1
        ;; C: Then jump to rCl
        je cond559
        ;; C: If not compile next cond in list
        ;; Cond: Else?
        ;; Is rbx true?
        cmp rbx, 1
        ;; Then jump to endCond label
        je endCond561
        ;; Else set rax to value of else
        mov rax, 2
        ;; End label
endCond561:
        ;; C: Jump for rClause
cond559:
        ;; C: Value of rClause
        mov rax, 2000
        ;; C: Update rbx with truthy val
        mov rbx, 1
        ret

;; rbx -> flag of if val set or not
;; if rbx is false and rax is true, update rbx with false and rax with rCl
;; if rbx is true, we're done
