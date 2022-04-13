#lang racket
(define (f . xs) xs) 
(define (append . xss)
   (if (empty? xss) '() (apply append (cdr xss) (cdr xss)))) 
(define (list . xs) xs)
(append (cons 1 (cons 2 (cons 3 '()))))

; (append (list 1 2 3))
; (append (list 2 3) (list 2 3)
; (append (list 3) (list 3))
; (append '() '())
