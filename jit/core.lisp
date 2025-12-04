;;;; JIT Core - Code Generation Nanopass
;;;;
;;;; This module provides THE code generation nanopass that transforms
;;;; IR to ARM64 machine code. Both deliver and REPL use this same codegen:
;;;;
;;;;   - deliver: jit-generate → macho.lisp (link) → Binary
;;;;   - REPL:    jit-generate → jit/execute.lisp (mmap+run) → Result
;;;;
;;;; The nanopass architecture:
;;;;   1. Read     (reader.lisp)      - S-expression parsing
;;;;   2. Compile  (compiler.lisp)    - IR generation
;;;;   3. Optimize (optimize.lisp)    - TCO, lambda lifting
;;;;   4. RegAlloc (reg-alloc.lisp)   - Register allocation
;;;;   5. Codegen  (jit/core.lisp)    - ARM64 code generation  <-- THIS FILE
;;;;   6. Link     (macho.lisp)       - Mach-O generation (deliver only)
;;;;
;;;; During SBCL bootstrap, this delegates to compiler-sbcl.lisp codegen.
;;;; When self-hosting, this contains the full implementation.

(in-package #:habu)

;;; ============================================================
;;; Code Generation Interface
;;; ============================================================

(defun jit-generate (ir ctx td)
  "Generate ARM64 code from IR using context CTX.
   TD is the temp slot depth for register spilling.
   Returns a list of bytes (and possibly markers in deliver mode).

   This is THE codegen nanopass - both deliver and REPL use this."
  (let ((rtaddrs (jit-context-fn-table ctx))
        (fnoffs (jit-context-fn-offsets ctx)))
    (codegen ir rtaddrs fnoffs td)))

;;; ============================================================
;;; Batch Code Generation (for deliver)
;;; ============================================================

(defun jit-generate-fn (fn ctx)
  "Generate ARM64 code for a function definition.
   FN is (name params body param-base) or lambda format.
   Returns code bytes with prologue/epilogue."
  (let ((rtaddrs (jit-context-fn-table ctx))
        (fnoffs (jit-context-fn-offsets ctx)))
    (codegen-fn fn rtaddrs fnoffs)))

(defun jit-generate-main (mir ctx)
  "Generate ARM64 code for main expression.
   Returns code bytes for the main entry point."
  (let ((rtaddrs (jit-context-fn-table ctx)))
    (codegen-main mir rtaddrs)))

(defun jit-generate-all-fns (fns ctx)
  "Generate ARM64 code for all functions.
   Returns (code . fn-offsets) where fn-offsets is alist of (name . offset)."
  (let ((rtaddrs (jit-context-fn-table ctx))
        (fnoffs (jit-context-fn-offsets ctx)))
    (codegen-all-fns fns rtaddrs fnoffs nil)))

;;; ============================================================
;;; Code Size Calculation
;;; ============================================================

(defun jit-code-size (code)
  "Calculate size of generated code in bytes.
   Handles nested lists and markers."
  (code-size code))
