;;;; Main Entry Point for Typed Compiler
;;;;
;;;; Provides deliver function that compiles s-expression to native binary.

(defpackage :habu.main
  (:use :cl)
  (:shadowing-import-from :habu.types :deftype :match :match*)
  (:export :deliver :compile-to-bytes :compile-to-function))

(in-package :habu.main)

;;; Load dependencies
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :habu.compile)
    (load "typed/compile.lisp"))
  (unless (find-package :habu.ir-to-tac)
    (load "typed/ir-to-tac.lisp"))
  (unless (find-package :habu.liveness)
    (load "typed/liveness.lisp"))
  (unless (find-package :habu.regalloc)
    (load "typed/regalloc.lisp"))
  (unless (find-package :habu.codegen)
    (load "typed/tac-codegen.lisp")))

;;; Compile a single expression to ARM64 bytes
(defun compile-to-bytes (expr)
  "Compile s-expression to ARM64 machine code bytes.
   Returns: list of bytes"
  (let* ((ir (habu.compile:compile-expr expr nil))
         (tac (habu.ir-to-tac:ir-to-tac ir))
         (alloc (habu.regalloc:allocate-registers tac))
         (code (habu.codegen:generate-code tac alloc)))
    code))

;;; Compile to function with proper prologue/epilogue
(defun compile-to-function (name params expr)
  "Compile expression to function bytes with prologue/epilogue.
   Returns: list of bytes"
  (let* ((ir (habu.compile:compile-expr expr nil))
         (tac (habu.ir-to-tac:ir-to-tac ir))
         (alloc (habu.regalloc:allocate-registers tac))
         (code (habu.codegen:codegen-function name params tac alloc)))
    code))

;;; Full pipeline to executable
(defun deliver (expr output-path &optional (heap-size #x4000000))
  "Compile expression to native ARM64 executable.
   Uses habu macho infrastructure for proper executable generation."
  (let ((code (compile-to-bytes expr)))
    (format t "Compiled ~S to ~D bytes~%" expr (length code))
    ;; Check if habu package is loaded for macho generation
    (if (find-package :habu)
        (let ((write-fn (intern "WRITE-MACHO-EXECUTABLE-WITH-IMPORTS-AND-HEAP" :habu)))
          (funcall write-fn output-path code '("_exit") heap-size nil nil)
          (format t "Wrote executable: ~A~%" output-path))
        ;; Fallback: write raw bytes
        (progn
          (with-open-file (f output-path
                             :direction :output
                             :element-type '(unsigned-byte 8)
                             :if-exists :supersede)
            (dolist (b code)
              (write-byte b f)))
          (format t "Wrote raw bytes: ~A (not executable)~%" output-path)))
    output-path))
