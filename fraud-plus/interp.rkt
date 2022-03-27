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
     (let* (
            (cls (interp-case-clauses e cs r))
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
                          [v v]))]))]

    ;; TODO: this works for just a single binding
    ;; but you need to make it work in general
    [(Let (list x) (list e1) e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v (interp-env e2 (ext r x v))])]

    ;; TODO: implement let, let*
    [(Let  xs es e) 'err]
    [(Let* xs es e) 
     (match xs
       ['() (interp-env e r)]
       [_ (interp-let* xs es e r)])]

    ;; TODO: remove this catch all, must be writing the code wrong
    [_ e]))

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


(define (interp-let* xs es e r)
  (match es
    ['() e]
    [(cons y ys) 
     (match xs 
       [(cons z zs) 
        (match (interp-env y r)
          ['err 'err]
          [v1 (let ((new-e (ext r z v1))) (interp-env (interp-let* zs ys e (append r new-e)) new-e))]
          [_ (list "y" y "ys" ys "z" z "zs" "xs" xs "es" es "e" e "r")])])]
    [_ (list "e" e "xs" xs "es" es "r" r )]))
          ;;[v1 (list "v1" v1 "y" y "ys" ys "z" z "zs" "xs" xs "es" es "e" e "r")])])])) 
              ;;[v2 (interp-env (interp-let* zs ys e r) (ext r z v2))])])])]))


;; Env Id -> Value
(define (lookup r x)
  (match r
    [(cons (list y val) r)
     (if (symbol=? x y)
         val
         (lookup r x))]
    [_ (list "lookup" "r" r "x" x)]))

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


(define (interp-case-clauses m e r)
  (match e
    [(cons x xs) (match x
                   ['err 'err]
                   [(Clause e1 e2) 
                    (let ((matchval (find-clause-match m e1 e2 r)))
                      (match (car matchval)
                             ['err (cons 'err 'err)]
                             [v (if (car matchval) 
                                  (cons #t (cdr matchval)) 
                                  (interp-case-clauses m xs r))]))]
                   ['() (cons #f #f)])]
    ['() (cons #f #f)]))

(define (find-clause-match m e1 e2 r)
  (match (interp-env m r)
    ['err (cons 'err 'err)]
    [v
      (match e1
    ;; if bool? or int?
    ;;   proceed w norm check
    ;; else
    ;;   err
      [(cons x xs)
       (if (or (boolean? x) (integer? x)) 
         (if (equal? v x)
           (cons #t e2);; if returned, interp in fx above
           (find-clause-match m xs e2 r))
         (cons 'err 'err))]
      ['() (cons #f #f)])]))
