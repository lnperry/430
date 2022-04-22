#lang racket
(define foo "foo")
(define (scoping x)
 foo)

(scoping "bar")
(let ((a "a")) (let ((a "b")) a))
