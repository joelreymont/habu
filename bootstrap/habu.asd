;;;; ASDF system definition for Habu bootstrap compiler

(defsystem "habu"
  :description "Habu Common Lisp bootstrap compiler"
  :version "0.1.0"
  :author "Joel Reymont"
  :license "MIT"
  :depends-on ()
  :serial nil
  ;; Treat warnings as errors for strict compilation
  :around-compile (lambda (next)
                    (handler-bind
                        ((style-warning
                          (lambda (c)
                            (error "Style warning treated as error: ~A" c))))
                      (funcall next)))
  :components
  (;; ARM64 assembler (must come first)
   (:module "arm64"
    :pathname "../arm64/"
    :components ((:file "asm")))

   ;; Core compiler (SBCL bootstrap)
   (:file "compiler-sbcl" :depends-on ("arm64"))

   ;; Optimization passes (depends on compiler for IR types)
   (:file "optimize" :depends-on ("compiler-sbcl"))

   ;; Register allocation nanopasses
   (:file "reg-alloc" :depends-on ("compiler-sbcl" "optimize"))

   ;; Mach-O linker (needs ARM64 asm)
   (:file "macho" :depends-on ("compiler-sbcl" "arm64"))

   ;; Reader
   (:file "reader" :depends-on ("compiler-sbcl"))

   ;; Habu self-hosted compiler (no SBCL deps)
   (:file "compiler" :depends-on ("compiler-sbcl"))

   ;; ARM64 code generator
   (:file "codegen" :depends-on ("compiler-sbcl" "optimize" "arm64"))

   ;; Mach-O utilities for native code
   (:file "macho-utils" :depends-on ("compiler-sbcl" "macho"))))

;;; Separate system for tests
(defsystem "habu/tests"
  :description "Habu compiler test suite"
  :depends-on ("habu")
  :components
  ((:file "test-harness")
   (:file "test-compiler" :depends-on ("test-harness"))))
