#lang racket

(define (f xs) xs) (apply f (cons 1 '()))
