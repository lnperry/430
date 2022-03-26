#lang racket
(provide tell)
(current-objs '("runtime.o"))
(require "parse.rkt" "compile.rkt" "types.rkt" a86/interp)
(define (tell e)
      (match (asm-interp (compile (parse e)))
        ['err 'err]
        [b (bits->value b)]))
