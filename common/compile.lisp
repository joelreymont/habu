;;;; Multi-Function Compilation
;;;; Pure Habu - no SBCL dependencies
;;;; Depends on: common/ir.lisp, common/utils.lisp

;;; Compile defun: (defun name (params) body) -> compiled function record
;;; Returns: (name params body-ir param-base)
;;; Note: This is a simplified version without optional/rest params
(defun compile-defun (name params body env fenv)
  (let* ((bindings (mapcar (lambda (p) (list p)) params))
         (param-env (env-extend bindings env))
         (param-base (if params
                         (env-lookup (car params) param-env)
                         0))
         ;; Add this function to fenv for recursive calls
         (recursive-fenv (cons (cons name nil) fenv))
         ;; Compile body in parameter environment
         (body-ir (compile-expr body param-env recursive-fenv)))
    (list name params body-ir param-base)))

;;; Compile list of forms, separating defuns from main expression
;;; Returns: (list-of-functions main-ir)
(defun compile-forms-helper (forms env fenv)
  (if (cons? forms)
      (let ((form (car forms)))
        (if (and (cons? form) (eq (car form) 'defun))
            ;; It's a defun
            (let* ((name (car (cdr form)))
                   (params (car (cdr (cdr form))))
                   (body (car (cdr (cdr (cdr form)))))
                   (compiled-fn (compile-defun name params body env fenv))
                   ;; Add to function environment
                   (new-fenv (cons (cons name compiled-fn) fenv))
                   ;; Compile rest of forms
                   (rest-result (compile-forms-helper (cdr forms) env new-fenv))
                   (rest-fns (car rest-result))
                   (main-ir (car (cdr rest-result))))
              ;; Return accumulated functions and main expression
              (list (cons compiled-fn rest-fns) main-ir))
            ;; Not a defun - this is the main expression
            (list nil (compile-expr form env fenv))))
      ;; No more forms
      (list nil (list 'lit 0))))

;;; Compile top-level forms
(defun compile-forms (forms)
  (compile-forms-helper forms nil nil))

;;; Calculate function code sizes and offsets
;;; Input: list of (name params body-ir param-base)
;;; Output: alist of (name . offset)
(defun calc-fn-offsets (fns prologue-size runtime-addrs)
  (labels ((iter (remaining offset acc)
             (if (nil? remaining)
                 (reverse acc)
                 (let* ((fn (car remaining))
                        (name (car fn))
                        (entry (cons name offset))
                        ;; Estimate function size (will be refined in codegen)
                        (fn-size 100))
                   (iter (cdr remaining)
                         (+ offset fn-size)
                         (cons entry acc))))))
    (iter fns prologue-size nil)))

;;; Generate code for a function with parameters
;;; Simplified version: fixed params only, no optional/rest
(defun codegen-function (fn runtime-addrs fn-offsets current-offset)
  (let* ((name (car fn))
         (params (car (cdr fn)))
         (body-ir (car (cdr (cdr fn))))
         (param-base (car (cdr (cdr (cdr fn)))))
         (nparams (length params))
         ;; Generate parameter store code
         (param-code (gen-param-stores params param-base 0 nil))
         (param-size (count-instrs param-code))
         ;; Body starts after param stores
         (body-offset (if current-offset (+ current-offset param-size) nil))
         ;; Generate body code
         (body-code (codegen-expr body-ir runtime-addrs fn-offsets body-offset 0)))
    (append param-code body-code (ret))))

;;; Generate parameter store instructions
(defun gen-param-stores (params base idx acc)
  (if (nil? params)
      acc
      (let* ((param-offset (* (+ base idx) 8))
             ;; x0-x4 hold args, store to stack
             (store (if (< idx 5)
                        (append
                         (mov-reg 22 idx)
                         (sub-imm 21 20 param-offset)
                         (str-offset 22 21 0))
                        ;; Args 5+ come from extra args pointer (x25)
                        (append
                         (ldr-offset 22 25 (* (- idx 5) 8))
                         (sub-imm 21 20 param-offset)
                         (str-offset 22 21 0)))))
        (gen-param-stores (cdr params) base (+ idx 1) (append acc store)))))

;;; Build complete program: functions + main
(defun codegen-program (fns main-ir runtime-addrs)
  (let* (;; Main prologue size (6 instructions)
         (prologue-size 6)
         ;; Calculate function offsets
         (fn-offsets (calc-fn-offsets fns prologue-size runtime-addrs))
         ;; Generate main with prologue
         (main-code (codegen-main-with-runtime main-ir runtime-addrs))
         ;; Cursor starts after main
         (main-size (count-instrs main-code))
         (fns-start main-size))
    ;; For now, just return main code
    ;; Full implementation would interleave function code
    main-code))

;;; Compile and generate code for program
(defun compile-program (forms runtime-addrs)
  (let* ((result (compile-forms forms))
         (fns (car result))
         (main-ir (car (cdr result))))
    (codegen-program fns main-ir runtime-addrs)))
