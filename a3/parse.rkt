#lang racket
(provide parse)
(require "ast.rkt")

;; S-Expr -> Expr
(define (parse s)
  (match s
    [(? integer?) (Int s)]
    [(? boolean?) (Bool s)]
    [(list 'add1 e)  (Prim1 'add1 (parse e))]
    [(list 'sub1 e)  (Prim1 'sub1 (parse e))]
    [(list 'abs e)   (Prim1 'abs  (parse e))]
    [(list '- e)     (Prim1 '-    (parse e))]
    [(list 'not e)   (Prim1 'not    (parse e))]
    [(list 'zero? e) (Prim1 'zero? (parse e))]
    [(list 'if e1 e2 e3)
     (If (parse e1) (parse e2) (parse e3))]
    [(list 'else e)  (parse e)]
    ;; thinking about writing this in a way that terminates
    ;; as in, doesnt continue to parse just ends there kind of thing
    [(list 'cond e ... s) (Cond (parse-clauses e) (parse s))]
    ;;[(list 'cond e) (parse e)]
    [(list p b) (Clause (parse p) (parse b))]
    ;; TODO: Handle cond
    ;; TODO: Handle case
    ;; TODO: Remove this clause once you've added clauses for
    ;; parsing cond and case; it's here just so running the test suite
    ;; doesn't trigger parse errors.
    [_ (Int 0)]
    [_ (error "parse error")]))


(define (parse-clauses s)
  (for/list ([i s])
    (parse i))
  )


