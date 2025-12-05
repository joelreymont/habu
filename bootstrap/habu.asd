;;;; ASDF system definition for Habu bootstrap compiler

(asdf:defsystem "habu"
  :description "Habu Common Lisp bootstrap compiler"
  :version "0.1.0"
  :author "Joel Reymont"
  :license "MIT"
  :depends-on ()
  :serial nil
  :in-order-to ((test-op (test-op "habu/tests")))
  :components
  (;; Package definitions (must come first)
   (:file "package")

   ;; ARM64 assembler
   (:module "arm64"
    :pathname "../arm64/"
    :depends-on ("package")
    :components ((:file "asm")))

   ;; Source-to-source expansions (shared by both compilers)
   (:file "expand" :depends-on ("package" "arm64"))

   ;; Core compiler (SBCL bootstrap)
   (:file "compiler-sbcl" :depends-on ("package" "arm64" "expand"))

   ;; Optimization passes (depends on compiler for IR types)
   (:file "optimize" :depends-on ("compiler-sbcl"))

   ;; Register allocation nanopasses
   (:file "reg-alloc" :depends-on ("compiler-sbcl" "optimize"))

   ;; Native ARM64 garbage collector
   (:file "gc" :depends-on ("arm64"))

   ;; Generational GC (builds on existing GC)
   (:file "gen-gc" :depends-on ("arm64" "gc"))

   ;; Mach-O linker (needs ARM64 asm)
   (:file "macho" :depends-on ("compiler-sbcl" "arm64"))

   ;; Reader
   (:file "reader" :depends-on ("compiler-sbcl"))

   ;; Habu self-hosted compiler (no SBCL deps)
   (:file "compiler" :depends-on ("compiler-sbcl"))

   ;; JIT module - shared code generation for REPL and deliver
   (:module "jit"
    :pathname "../jit/"
    :depends-on ("compiler-sbcl" "arm64" "gc")
    :components
    ((:file "context")
     (:file "core" :depends-on ("context"))
     (:file "execute" :depends-on ("core"))))

   ;; ARM64 code generator (uses JIT core)
   (:file "codegen" :depends-on ("compiler-sbcl" "optimize" "arm64" "gc" "jit"))

   ;; Mach-O utilities for native code
   (:file "macho-utils" :depends-on ("compiler-sbcl" "macho"))

   ;; Trace facility
   (:file "trace" :depends-on ("compiler-sbcl"))

   ;; JIT executor (subprocess-based)
   (:file "executor" :depends-on ("compiler-sbcl" "codegen" "macho"))

   ;; FASL format for separate compilation
   (:file "fasl" :depends-on ("compiler-sbcl" "codegen" "macho"))))

;;;; Test system
(asdf:defsystem "habu/tests"
  :description "Habu compiler test suite"
  :depends-on ("habu")
  :pathname ""
  :components
  ((:file "test-harness")
   (:module "tests"
    :pathname "../tests/"
    :depends-on ("test-harness")
    :components
    ((:file "test-core")
     (:file "test-keyword-args" :depends-on ("test-core"))
     (:file "test-packages" :depends-on ("test-core"))
     (:file "test-reader" :depends-on ("test-core"))
     (:file "test-undefined" :depends-on ("test-core"))
     (:file "test-match" :depends-on ("test-core"))
     (:file "test-macros" :depends-on ("test-core"))
     (:file "quickcheck" :depends-on ("test-core"))
     (:file "test-arm64-props" :depends-on ("quickcheck"))
     (:file "test-match-props" :depends-on ("quickcheck"))
     (:file "test-fasl" :depends-on ("quickcheck")))))
  :perform (asdf:test-op (o c)
             (declare (ignore o c))
             (uiop:symbol-call :habu-test '#:run-all-tests)))
