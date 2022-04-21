#lang racket
(require "test-runner.rkt"
         "../parse.rkt"
         "../compile.rkt"
         "../unload-bits-asm.rkt"
         a86/interp)

;; link with runtime for IO operations
(unless (file-exists? "../runtime.o")
  (system "make -C .. runtime.o"))
(current-objs
 (list (path->string (normalize-path "../runtime.o"))))

(test-runner    (λ p
                  (with-handlers ([exn? (lambda (e) e)])
                    (unload/free (asm-interp (compile (parse p)))))))
(test-runner-io (λ (s . p)
                  (with-handlers ([exn? (lambda (e) e)])
                    (match (asm-interp/io (compile (parse p)) s)
                      ['err 'err]
                      [(cons r o) (cons (unload/free r) o)]))))
