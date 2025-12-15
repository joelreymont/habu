;;;; Main Entry Point for Typed Compiler
;;;;
;;;; Provides deliver function that compiles s-expression to native binary.

(defpackage :habu.main
  (:use :cl)
  (:shadowing-import-from :habu.types :deftype :match :match*)
  (:export :deliver :compile-to-bytes :compile-to-function :compile-defun))

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
(defun compile-to-function (name params body)
  "Compile expression to function bytes with prologue/epilogue.
   Returns: list of bytes"
  (let* ((env (make-param-env params))
         (ir (habu.compile:compile-expr body env))
         (tac (habu.ir-to-tac:ir-to-tac ir))
         (alloc (habu.regalloc:allocate-registers tac))
         (code (habu.codegen:codegen-function name params tac alloc)))
    code))

(defun make-param-env (params)
  "Create environment mapping params to stack offsets."
  (let ((env nil)
        (offset 0))
    (dolist (p params)
      (push (cons p offset) env)
      (incf offset))
    (nreverse env)))

;;; Compile a defun form
(defun compile-defun (form)
  "Compile (defun name (params) body...) to function bytes.
   Returns: (name . bytes)"
  (unless (and (consp form)
               (eq (car form) 'defun)
               (>= (length form) 4))
    (error "Invalid defun form: ~S" form))
  (let* ((name (second form))
         (params (third form))
         (body (if (= (length (cdddr form)) 1)
                   (fourth form)
                   (cons 'progn (cdddr form))))
         (code (compile-to-function name params body)))
    (cons name code)))

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
