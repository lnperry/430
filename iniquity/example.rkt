#lang racket

(define (f x) (let ((y 2)) (empty? x))) 
(f (let ((x 2)) (add1 (car (cons (+ 1 2) (cons 1 2))))))
