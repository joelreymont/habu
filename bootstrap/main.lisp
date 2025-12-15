;;;; Main Entry Point for Compiler
;;;;
;;;; Provides deliver function that compiles s-expression to native binary.

(defpackage :habu.main
  (:use :cl)
  (:shadowing-import-from :habu.types :deftype :match :match*)
  (:import-from :habu.ir
                :defun-fn :defun-fn-name :defun-fn-params :defun-fn-body :defun-fn-param-base
                :cr-result :cr-result-defuns :cr-result-main-ir)
  (:import-from :habu.compile
                :lift-lambdas-typed :lift-lambdas-from-defuns-typed
                :reset-typed-lambda-counter)
  (:export :deliver :deliver-forms-typed :compile-to-bytes :compile-to-function :compile-defun
           :codegen-defun-ir :codegen-ir :codegen-all-defuns))

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
   Returns: list of bytes (without function prologue/epilogue)"
  (let* ((tac-full (habu.ir-to-tac:ir-to-tac ir))
         ;; Strip TAC-RETURN so we can wrap with our own main wrapper
         (tac (strip-tac-return tac-full))
         (alloc (habu.regalloc:allocate-registers tac))
         (code (habu.codegen:generate-code tac alloc)))
    ;; Ensure result ends up in x0 for main wrapper
    (append code (move-result-to-x0 alloc))))

(defun codegen-defun-ir (defun-ir)
  "Compile a defun-ir to ARM64 bytes with proper prologue/epilogue.
   Input: defun-ir from compile-forms (name params body param-base)
   Returns: (values code-bytes markers) where markers is list of (offset marker-data)"
  (let* ((name (defun-fn-name defun-ir))
         (params (defun-fn-params defun-ir))
         (body-ir (defun-fn-body defun-ir))
         (param-base (defun-fn-param-base defun-ir))
         ;; Convert typed IR body to TAC
         (tac (habu.ir-to-tac:ir-to-tac body-ir))
         ;; Allocate registers
         (alloc (habu.regalloc:allocate-registers tac)))
    (declare (ignore param-base))  ; TODO: handle captures
    ;; Generate function code with prologue/epilogue
    (habu.codegen:codegen-function name params tac alloc)))

;;; ============================================================
;;; Typed Deliver Pipeline
;;; ============================================================
;;;
;;; Full compilation from source forms to executable using typed IR.

(defun codegen-all-defuns (defuns)
  "Compile all defun-ir to bytes with function offsets.
   Returns (values fn-alist all-bytes all-markers) where:
   - fn-alist maps names to byte offsets
   - all-bytes is the concatenated code
   - all-markers is list of (abs-offset marker-data) for all functions"
  (let ((fn-alist nil)
        (all-bytes nil)
        (all-markers nil)
        (offset 0))
    (dolist (dfn defuns)
      (let ((name (defun-fn-name dfn)))
        (multiple-value-bind (code markers)
            (codegen-defun-ir dfn)
          (let ((size (length code)))  ; Code is now flat bytes
            ;; Record function at current offset
            (push (cons name offset) fn-alist)
            ;; Adjust marker offsets and collect
            (dolist (m markers)
              (push (list (+ offset (first m)) (second m)) all-markers))
            ;; Append code
            (setf all-bytes (append all-bytes code))
            ;; Update offset
            (incf offset size)))))
    (values (reverse fn-alist) all-bytes (reverse all-markers))))

(defun count-code-size (code)
  "Count the actual byte size of code (handling markers)."
  (let ((size 0))
    (dolist (item code)
      (if (and (consp item) (keywordp (car item)))
          ;; Marker takes 4 bytes (BL instruction)
          (incf size 4)
          ;; Regular byte
          (incf size)))
    size))

(defun resolve-call-markers (code markers fn-alist code-start-offset)
  "Resolve call markers by patching placeholder bytes.
   code: list of bytes (with placeholder bytes DE AD BE EF at marker positions)
   markers: list of (offset (:call-fn name)) relative to this code segment
   fn-alist: maps function names to absolute byte offsets in final binary
   code-start-offset: where this code segment starts in final binary
   Returns (values resolved-code unresolved-markers) where unresolved-markers
   contains external function calls that need linker resolution."
  (if (null markers)
      (values code nil)
      (let ((code-vec (coerce code 'vector))
            (unresolved nil))
        (dolist (marker markers)
          (let* ((marker-offset (first marker))
                 (marker-data (second marker))
                 (fn-name (second marker-data))  ; (:call-fn name) -> name
                 (fn-abs-offset (cdr (assoc fn-name fn-alist :test #'equal))))
            (if fn-abs-offset
                ;; Known local function - resolve to BL
                (let* ((current-abs (+ code-start-offset marker-offset))
                       (rel (- fn-abs-offset current-abs))
                       (rel-instrs (ash rel -2))
                       (bl-bytes (arm64:bl rel-instrs)))
                  (loop for i from 0 below 4
                        for b in bl-bytes
                        do (setf (aref code-vec (+ marker-offset i)) b)))
                ;; Unknown function - leave placeholder, track for linker
                (push (list (+ code-start-offset marker-offset) fn-name) unresolved))))
        (values (coerce code-vec 'list) (nreverse unresolved)))))

(defun deliver-forms-typed (forms output-path &optional (heap-size #x4000000))
  "Compile forms to native executable using typed IR pipeline.
   This is the typed replacement for deliver-forms in codegen.lisp."
  (reset-typed-lambda-counter)

  ;; Compile forms to typed IR
  (let* ((result (habu.compile:compile-forms forms))
         (defuns-orig (cr-result-defuns result))
         (main-ir-orig (cr-result-main-ir result)))

    ;; Lift lambdas from main IR
    (let* ((main-lift (lift-lambdas-typed main-ir-orig nil))
           (main-ir (car main-lift))
           (main-lambdas (cdr main-lift)))

      ;; Lift lambdas from defun bodies
      (let* ((defun-lift (lift-lambdas-from-defuns-typed defuns-orig nil nil))
             (defuns (car defun-lift))
             (defun-lambdas (cdr defun-lift)))

        ;; Combine all functions (defuns + lifted lambdas)
        (let ((all-defuns (append defuns main-lambdas defun-lambdas)))
          (multiple-value-bind (fn-alist fn-bytes fn-markers)
              (codegen-all-defuns all-defuns)
            ;; Generate main code with proper prologue/epilogue
            (let* ((main-tac (habu.ir-to-tac:ir-to-tac main-ir))
                   (main-alloc (habu.regalloc:allocate-registers main-tac)))
              (multiple-value-bind (main-code main-markers)
                  (habu.codegen:codegen-function '_main nil main-tac main-alloc)
                (let* ((main-size (length main-code))
                       (fn-start-offset main-size)
                       ;; Adjust fn-alist to absolute offsets
                       (fn-alist-abs (mapcar (lambda (e)
                                               (cons (car e) (+ fn-start-offset (cdr e))))
                                             fn-alist))
                       ;; Combine all markers
                       (all-markers (append main-markers
                                            (mapcar (lambda (m)
                                                      (list (+ main-size (first m)) (second m)))
                                                    fn-markers)))
                       ;; Combine main + functions
                       (all-code-raw (append main-code fn-bytes)))
                  ;; Resolve call markers
                  (multiple-value-bind (all-code unresolved)
                      (resolve-call-markers all-code-raw all-markers fn-alist-abs 0)
                    (when unresolved
                      (format t "Warning: ~D unresolved external calls~%" (length unresolved)))

                    ;; Calculate heap page offset for wrapper
                    ;; Layout: code at 0x400, wrapper adds 172 bytes
                    ;; After text segment comes DATA_CONST (1 page), then heap
                    (let* ((wrapper-size 172)  ; +heap-wrapper-size+
                           (code-offset #x400)
                           (stubs-size 12)     ; Single _exit stub
                           (text-end (+ code-offset wrapper-size (length all-code) stubs-size))
                           (text-vmsize (* (ceiling text-end #x4000) #x4000))
                           (text-pages-4kb (/ text-vmsize #x1000))
                           (heap-page-offset (+ text-pages-4kb 4)))

                      (format t "Compiled ~D functions, main ~D bytes, total ~D bytes~%"
                              (length all-defuns) main-size (length all-code))

                      ;; Wrap with heap initialization and write
                      (if (find-package :habu)
                          (let ((wrap-fn (intern "WRAP-BYTECODE-WITH-HEAP-FOR-IMPORTS" :habu))
                                (write-fn (intern "WRITE-MACHO-EXECUTABLE-WITH-IMPORTS-AND-HEAP" :habu)))
                            (let ((wrapped-code (funcall wrap-fn all-code heap-page-offset 0 0)))
                              (funcall write-fn output-path wrapped-code '("_exit") heap-size nil nil))
                            (format t "Wrote executable: ~A~%" output-path))
                          (progn
                            (with-open-file (f output-path
                                               :direction :output
                                               :element-type '(unsigned-byte 8)
                                               :if-exists :supersede)
                              (dolist (b all-code)
                                (write-byte b f)))
                            (format t "Wrote raw bytes: ~A~%" output-path)))
                      output-path)))))))))))
