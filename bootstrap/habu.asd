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
  (;; Host compatibility layer (loaded FIRST, before package shadows CL functions)
   (:file "host-compat")
   (:file "host-sbcl" :depends-on ("host-compat"))

   ;; Type system (ADT definitions) - MUST load before package for code-marker import
   (:file "../shared/types" :depends-on ("host-sbcl"))

   ;; Package definitions - imports from habu.types
   (:file "package" :depends-on ("host-sbcl" "../shared/types"))

   ;; Tag constants (SINGLE SOURCE OF TRUTH for hybrid 1+3 bit tagging)
   (:file "../shared/tags" :depends-on ("package"))

   ;; Shared macros (while, incf, decf) - needed before reg-alloc
   (:file "../shared/macros" :depends-on ("package"))

   ;; IR ADT definitions (frame-layout, lambda-entry, etc.)
   (:file "../shared/ir" :depends-on ("../shared/types"))

   ;; ARM64 assembler
   (:module "arm64"
    :pathname "../arm64/"
    :depends-on ("package")
    :components ((:file "asm")))

   ;; ARM64 tag operation helpers (combines tags.lisp constants with arm64 encoding)
   (:file "../shared/tag-ops" :depends-on ("../shared/tags" "arm64"))

   ;; Source-to-source expansions (shared by both compilers)
   (:file "expand" :depends-on ("package" "arm64"))

   ;; Core compiler (SBCL bootstrap)
   (:file "compiler-sbcl" :depends-on ("package" "arm64" "expand"))

   ;; Optimization passes (depends on compiler for IR types)
   (:file "optimize" :depends-on ("compiler-sbcl"))

   ;; Register allocation nanopasses
   (:file "reg-alloc" :depends-on ("compiler-sbcl" "optimize" "../shared/macros" "../shared/ir"))

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
   (:file "codegen" :depends-on ("compiler-sbcl" "optimize" "arm64" "gc" "jit" "../shared/ir"))

   ;; Mach-O utilities for native code
   (:file "macho-utils" :depends-on ("compiler-sbcl" "macho"))

   ;; Trace facility
   (:file "trace" :depends-on ("compiler-sbcl"))

   ;; JIT executor (subprocess-based)
   (:file "executor" :depends-on ("compiler-sbcl" "codegen" "macho"))

   ;; FASL format for separate compilation
   (:file "fasl" :depends-on ("compiler-sbcl" "codegen" "macho"))

   ;; DWARF debug info generation
   (:file "dwarf" :depends-on ("package"))

   ;; Debug info infrastructure
   (:file "debug-info" :depends-on ("dwarf"))

   ;; Source location tracking
   (:file "source-locations" :depends-on ("compiler-sbcl" "debug-info"))

   ;; === TYPED COMPILER PIPELINE ===
   ;; These modules use exhaustive ADT matching for type safety

   ;; IR ADT - habu.ir package for typed IR nodes
   (:file "ir" :depends-on ("../shared/types"))

   ;; TAC ADT - Three Address Code intermediate representation
   (:file "tac" :depends-on ("../shared/types" "ir"))

   ;; Typed front-end: S-expression to IR
   (:file "compile" :depends-on ("../shared/types" "ir"))

   ;; IR to TAC conversion with typed pattern matching
   (:file "ir-to-tac" :depends-on ("../shared/types" "ir" "tac"))

   ;; Liveness analysis for register allocation
   (:file "liveness" :depends-on ("tac"))

   ;; Register allocation with typed TAC
   (:file "regalloc" :depends-on ("tac" "liveness"))

   ;; TAC to ARM64 code generation with exhaustive matching
   (:file "tac-codegen" :depends-on ("../shared/types" "tac" "regalloc" "arm64"))

   ;; Main entry point with deliver-forms-typed
   (:file "main" :depends-on ("compile" "ir-to-tac" "regalloc" "tac-codegen" "macho"))))

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
     (:file "test-block" :depends-on ("test-core"))
     (:file "test-linearize" :depends-on ("test-core"))
     (:file "quickcheck" :depends-on ("test-core"))
     (:file "test-arm64-props" :depends-on ("quickcheck"))
     (:file "test-match-props" :depends-on ("quickcheck"))
     (:file "test-fasl" :depends-on ("quickcheck"))))
   ;; Debug infrastructure tests (outside tests module)
   (:file "test-debug" :depends-on ("test-harness")))
  :perform (asdf:test-op (o c)
             (declare (ignore o c))
             (uiop:symbol-call :habu-test '#:run-all-tests)))
