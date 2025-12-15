;;;; Main Entry Point for Compiler
;;;;
;;;; Provides deliver function that compiles s-expression to native binary.

(defpackage :habu.main
  (:use :cl)
  (:shadowing-import-from :habu.types :deftype :match :match*)
  (:import-from :habu.ir
                :defun-fn-name :defun-fn-params :defun-fn-body :defun-fn-param-base
                :cr-result-defuns :cr-result-main-ir)
  (:export :deliver :compile-to-bytes :compile-to-function :compile-defun
           :codegen-defun-ir :codegen-ir))

(in-package :habu.main)

;;; Load dependencies
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :habu.compile)
    (load "bootstrap/compile.lisp"))
  (unless (find-package :habu.ir-to-tac)
    (load "bootstrap/ir-to-tac.lisp"))
  (unless (find-package :habu.liveness)
    (load "bootstrap/liveness.lisp"))
  (unless (find-package :habu.regalloc)
    (load "bootstrap/regalloc.lisp"))
  (unless (find-package :habu.codegen)
    (load "bootstrap/tac-codegen.lisp")))

;;; Compile a single expression to ARM64 bytes
(defun compile-to-bytes (expr)
  "Compile s-expression to ARM64 machine code bytes.
   Returns: list of bytes (expression code without prologue/epilogue)"
  (let* ((ir (habu.compile:compile-expr expr nil))
         (tac-full (habu.ir-to-tac:ir-to-tac ir))
         ;; Strip the TAC-RETURN that ir-to-tac adds (we'll add our own wrapper)
         (tac (strip-tac-return tac-full))
         (alloc (habu.regalloc:allocate-registers tac))
         (code (habu.codegen:generate-code tac alloc)))
    ;; Ensure result ends up in x0
    (append code (move-result-to-x0 alloc))))

(defun strip-tac-return (tac-instrs)
  "Remove trailing TAC-RETURN instruction if present."
  (if (and (consp tac-instrs)
           (consp (car (last tac-instrs)))
           (eq (car (car (last tac-instrs))) :tac-return))
      (butlast tac-instrs)
      tac-instrs))

(defun move-result-to-x0 (alloc)
  "Generate code to move the final result vreg to x0 if needed.
   The result is in the highest-numbered vreg.
   Returns: list of bytes (mov instruction if needed, empty otherwise)"
  (let* ((vreg-to-reg (habu.regalloc:allocation-result-vreg-to-reg alloc))
         ;; Find the highest vreg number (that's the final result)
         (max-vreg (loop for vreg being the hash-keys of vreg-to-reg
                         maximize vreg))
         (result-reg-num (gethash max-vreg vreg-to-reg)))
    (if (and result-reg-num (not (= result-reg-num 0)))
        ;; Result is not in x0, need to move it
        (arm64:mov :x0 (arm64:num-to-reg result-reg-num))
        ;; Result already in x0 or no result
        nil)))

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
  (let* ((expr-code (compile-to-bytes expr))
         ;; Wrap expression code with main entry point
         (main-code (wrap-expr-as-main expr-code)))
    (format t "Compiled ~S to ~D bytes (~D expr + ~D wrapper)~%"
            expr (length main-code) (length expr-code) (- (length main-code) (length expr-code)))
    ;; Check if habu package is loaded for macho generation
    (if (find-package :habu)
        (let ((write-fn (intern "WRITE-MACHO-EXECUTABLE-WITH-IMPORTS-AND-HEAP" :habu)))
          (funcall write-fn output-path main-code '("_exit") heap-size nil nil)
          (format t "Wrote executable: ~A~%" output-path))
        ;; Fallback: write raw bytes
        (progn
          (with-open-file (f output-path
                             :direction :output
                             :element-type '(unsigned-byte 8)
                             :if-exists :supersede)
            (dolist (b main-code)
              (write-byte b f)))
          (format t "Wrote raw bytes: ~A (not executable)~%" output-path)))
    output-path))

(defun wrap-expr-as-main (expr-bytes)
  "Wrap expression code with main function wrapper.
   - Sets up x28 (heap) and x20 (env) registers
   - Calls expression code
   - Untagges result and exits via _exit syscall
   Returns: list of bytes for complete main function"
  (let ((prologue nil)
        (epilogue nil))
    ;; Prologue: minimal stack frame setup (no need for full 16KB frame)
    (setf prologue
          (append
           ;; Save frame pointer and link register
           (arm64:sub :sp :sp #x10 :imm t)           ; sub sp, sp, #16
           (arm64:str :x29 :sp :offset 0)             ; str x29, [sp]
           (arm64:str :x30 :sp :offset 8)             ; str x30, [sp, 8]
           (arm64:mov :x29 :sp)))                     ; mov x29, sp

    ;; Epilogue: extract result, restore, and exit
    (setf epilogue
          (append
           ;; Result is in x0 (already there from expression)
           ;; Untag fixnum: x0 = x0 >> 1
           (arm64:asr :x0 :x0 1 :imm t)               ; asr x0, x0, #1
           ;; Restore frame
           (arm64:ldr :x29 :sp :offset 0)             ; ldr x29, [sp]
           (arm64:ldr :x30 :sp :offset 8)             ; ldr x30, [sp, 8]
           (arm64:add :sp :sp #x10 :imm t)            ; add sp, sp, #16
           ;; Exit syscall: x16=1 (exit), x0=exit_code, svc 0
           (arm64:movz :x16 1)                        ; movz x16, #1
           (arm64:svc 0)))                            ; svc #0

    ;; Concatenate: prologue + expr + epilogue
    (append prologue expr-bytes epilogue)))

;;; ============================================================
;;; Typed Pipeline Integration
;;; ============================================================
;;;
;;; These functions integrate with the typed IR from compile-forms.

(defun codegen-ir (ir)
  "Compile typed IR to ARM64 bytes using the typed pipeline.
   Returns: list of bytes"
  (let* ((tac (habu.ir-to-tac:ir-to-tac ir))
         (alloc (habu.regalloc:allocate-registers tac))
         (code (habu.codegen:generate-code tac alloc)))
    code))

(defun codegen-defun-ir (defun-ir)
  "Compile a defun-ir to ARM64 bytes with proper prologue/epilogue.
   Input: defun-ir from compile-forms (name params body param-base)
   Returns: list of bytes for complete function"
  (let* ((name (defun-fn-name defun-ir))
         (params (defun-fn-params defun-ir))
         (body-ir (defun-fn-body defun-ir))
         (param-base (defun-fn-param-base defun-ir))
         ;; Convert typed IR body to TAC
         (tac (habu.ir-to-tac:ir-to-tac body-ir))
         ;; Allocate registers
         (alloc (habu.regalloc:allocate-registers tac))
         ;; Generate function code with prologue/epilogue
         (code (habu.codegen:codegen-function name params tac alloc)))
    (declare (ignore param-base))  ; TODO: handle captures
    code))
