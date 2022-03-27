#lang racket
(provide interp interp-env)
(require "ast.rkt" "interp-prim.rkt")

;; type Answer = Value | 'err

;; type Value =
;; | Integer
;; | Boolean
;; | Character
;; | Eof
;; | Void

;; type REnv = (Listof (List Id Value))

;; HINT: this is a function that may come in handy.
;; It takes a list of expressions and environment
;; and evaluates each expression in order.  If any
;; expression produces 'err, the whole thing produces
;; 'err; otherwise it produces a list of values.

(define (foldl-err proc proc2 init lst)
  (match lst
    ['() init]
    [(cons x xs) 
      (match x 
        [(? proc2) (foldl-err proc
                        (proc (interp x) init)
                        xs)]
        [_ 'err])]))

;; Expr -> Answer
(define (interp e)
  (interp-env e '()))

;; Expr Env -> Answer
(define (interp-env e r)
  (match e
    [(Int i) i]
    [(Bool b) b]
    [(Char c) c]
    [(Eof) eof]
    [(Var x) (lookup r x)]
    [(Prim0 p) (interp-prim0 p)]
    [(Prim1 p e)
     (match (interp-env e r)
       ['err 'err]
       [v (interp-prim1 p v)])]
    [(Prim2 p e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v1 (match (interp-env e2 r)
             ['err 'err]
             [v2 (interp-prim2 p v1 v2)])])]
    ;; TODO: implement n-ary primitive +
    ;; rewrite PrimN in terms of Prim2?
    [(PrimN p es) 
     (match p
       ['+ (interp-primn p es r)])]
      
    [(If p e1 e2)
     (match (interp-env p r)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r)
            (interp-env e2 r))])]
    [(Begin e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v    (interp-env e2 r)])]

    ;; TODO: implement cond
    [(Cond cs e)
     (let* ([cls (interp-cond-clauses cs r)]
            [first (car cls)]
            [second (cdr cls)])
            ;;(list cls first second x))]
            (match first
                   ['err 'err]
                   [v (if first 
                        (match (interp-env second r)
                          ['err 'err]
                          [v v])
                        (match (interp-env e r)
                          ['err 'err]
                          ;; wait interp else here "AST long as possible"
                          [v v]))]))] 
    ;; TODO: implement case
    [(Case e cs el)
     (match (interp-env e r)
       ['err 'err]
       [m 
         (let* (
           (cls (interp-case-clauses m cs))
           (first (car cls))
           (second (cdr cls)))
             (match first
              ['err 'err]
              [v (if first
                   (match (interp-env second r)
                          ['err 'err]
                          [v v])
                   (match (interp-env el r)
                          ['err 'err]
                          [v v]))]))])]

    ;; TODO: this works for just a single binding
    ;; but you need to make it work in general
    [(Let (list x) (list e1) e2)
     (interp-let x e1 e2 r)]

    ;; TODO: implement let, let*
    ;; NOTE: think can do interp-env* on es, to verify no bindings
    ;; and then just call Let*?
    ;; Is there a better way?
    ;; Does it matter if there is a better way?
    ;; JOSE: "implement first, optimize later"
    [(Let  xs es e) 
      (match (interp*-env es r) ;; check no unbinded vars
        ['err 'err]
        [_ (interp-env (translate-let* (map cons xs es) e r) r)])]

    ;; (interp-env (translate-let* (map cons xs es) e r) r)]

    [(Let* xs es e) 
     (interp-env (translate-let* (map cons xs es) e r) r)]

    ;; NOTE: you must add this here if you're passing the AST back into interp
    ;; NOTE: adding this back for case issue
    [_ e]))

;; type Answer* = 'err | [Listof Value]
;; [Listof Expr] Env -> Answer*
(define (interp*-env es r)
  (match es
    ['() '()]
    [(cons e es)
     (match (interp-env e r)
       ['err 'err]
       [v (match (interp*-env es r)
            ['err 'err]
            [vs (cons v vs)])])]))

(define (interp-let x e1 e2 r)
  (match (interp-env e1 r)
    ['err 'err]
    [v (interp-env e2 (ext r x v))]))

(define (translate-let* pairs e r)
    (match pairs
           ['() e]
           [(list a) (Let (list (car a)) (list (cdr a)) e)]
           [(cons x xs) (Let (list (car x)) (list (cdr x)) (translate-let* xs e r))]))

(define (interp-primn p es r)
  (match es
    ['() 0]
    [(list x) (match (interp-env x r)
                  ['err 'err]
                  [v1 v1])]
    [(list x y) (match (interp-env x r)
                  ['err 'err]
                  [v1 (match (interp-env y r)
                             ['err 'err]
                             [v2 (interp-prim2 p v1 v2)])])]
    [(cons x xs) (match (interp-env x r)
                   ['err 'err]
                   [v1 (interp-prim2 p v1 (interp-primn p xs r))])]))




;; Env Id -> Value
(define (lookup r x)
  (match r
    [(cons (list y val) r)
     (if (symbol=? x y)
         val
         (lookup r x))]
    [_ 'err]))

;; Env Id Value -> Env
(define (ext r x v)
  (cons (list x v) r))

(define (interp-cond-clauses e r)
  (match e
    [(cons x xs) (match x
                   [(Clause e1 e2) 
                    ;; if err
                    ;;   return err
                    ;; else
                    ;;   if true
                    ;;     (cons true)
                    ;;   else
                    ;;     (recurse)
                    (match (interp-env e1 r)
                               ['err (cons 'err 'err)]
                               [v (if v 
                                    (cons #t e2)
                                    (interp-cond-clauses xs r))])]
		      ;; consider (If e1 e2 e3) from our language
		      ;; if e2 is false, then this will break bc it will return the else
                   ['() (cons #f #f)])]
    ['() (cons #f #f)]))


(define (interp-case-clauses m cs)
  (match cs
    [(cons x xs) 
     (match x
       [(Clause e1 e2) 
         (let ((matchval (find-clause-match m e1 e2)))
           (if (car matchval) 
            (cons #t (cdr matchval)) 
            (interp-case-clauses m xs)))]
       ['() (cons #f #f)])]
    ['() (cons #f #f)]))

(define (find-clause-match m e1 e2)
  (match e1
    [(cons x xs) 
     (if (equal? m (interp x))
       (cons #t e2);; if returned, interp in fx above
       (find-clause-match m xs e2))]
    ['() (cons #f #f)]))

















