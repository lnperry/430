#lang racket
(provide tell)
(current-objs '("runtime.o"))
(require "parse.rkt" "interp.rkt" "compile.rkt" "types.rkt" a86/interp rackunit a86/printer "ast.rkt")
(define (tell e)
      (match (asm-interp (compile (parse e)))
        ['err 'err]
        [b (bits->value b)]))
