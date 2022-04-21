#lang racket
(require "test-runner.rkt"
         "build-runtime.rkt"
         "../parse.rkt"
         "../compile.rkt"
         "../unload-bits-asm.rkt"
         a86/interp)

(test-runner    (λ (e)
                  (with-handlers ([exn? (lambda (e) 'CRASH)])
                    (unload/free (asm-interp (compile (parse e)))))))
(test-runner-io (λ (e s)
                  (with-handlers ([exn? (lambda (e) 'CRASH)])
                    (match (asm-interp/io (compile (parse e)) s)
                      ['err 'err]
                      [(cons r o) (cons (unload/free r) o)]))))
