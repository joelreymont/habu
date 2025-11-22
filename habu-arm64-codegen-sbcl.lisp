;;;; SBCL-only loader stubs for Habu codegen (keeps main file standalone)
;;;; Do NOT use in production; only for bring-up/testing in SBCL host

(defpackage :habu-sbcl-codegen
  (:use :cl :habu-shim)
  (:export codegen-expr compile-expr compile-to-arm64-with-runtime compile-to-arm64
           make-runtime-addrs runtime-lookup *runtime-addrs*
           compile-program-with-functions-with-runtime compile-program-with-functions))

(in-package :habu-sbcl-codegen)

(defparameter *runtime-addrs* nil)

(defun encode-word-le (word)
  "Encode 32-bit word into little-endian byte list for smoke output."
  (list (logand word #xFF)
        (logand (ash word -8) #xFF)
        (logand (ash word -16) #xFF)
        (logand (ash word -24) #xFF)))

(defun pick-runtime-imm (runtime-addrs fallback)
  "Choose a low 16-bit immediate from runtime-addrs (alist), else fallback."
  (let ((entry (car runtime-addrs)))
    (if entry
        (logand (cdr entry) #xFFFF)
        (logand fallback #xFFFF))))

(defun has-tag? (ir tag)
  (and (consp ir) (eq (car ir) tag)))

(defun env-lookup (sym env)
  (declare (ignore sym env))
  nil)

(defun runtime-lookup (name runtime-addrs)
  "SBCL shim: lookup name in alist runtime-addrs (symbol . addr)."
  (if (nil? runtime-addrs)
      #x0
      (let* ((entry (car runtime-addrs))
             (entry-name (car entry))
             (entry-addr (cdr entry)))
        (if (eq name entry-name)
            entry-addr
            (runtime-lookup name (cdr runtime-addrs))))))

(defun make-runtime-addrs (cons-addr car-addr cdr-addr)
  "Create runtime address table for cons/car/cdr."
  (list (cons 'habu_cons cons-addr)
        (cons 'habu_car car-addr)
        (cons 'habu_cdr cdr-addr)))

;; ARM64 Instruction Encoders (Functional)
(defun arm64-movz (rd imm)
  "MOVZ Xd, #imm16 - Move zero-extended 16-bit immediate"
  (let* ((imm16 (logand imm #xFFFF))
         (base #xD2800000)
         (encoded (logior base (ash imm16 5) rd)))
    (encode-word-le encoded)))

(defun arm64-add (rd rn rm)
  "ADD Xd, Xn, Xm - Add registers"
  (let* ((base #x8B000000)
         (encoded (logior base (ash rm 16) (ash rn 5) rd)))
    (encode-word-le encoded)))

(defun arm64-sub (rd rn rm)
  "SUB Xd, Xn, Xm - Subtract registers"
  (let* ((base #xCB000000)
         (encoded (logior base (ash rm 16) (ash rn 5) rd)))
    (encode-word-le encoded)))

(defun arm64-mul (rd rn rm)
  "MUL Xd, Xn, Xm - Multiply registers"
  (let* ((base #x9B007C00)
         (encoded (logior base (ash rm 16) (ash rn 5) rd)))
    (encode-word-le encoded)))

(defun arm64-lsl (rd rn shift)
  "LSL Xd, Xn, #shift - Logical shift left immediate"
  (let* ((base #xD3400000)
         (shift-bits (logand shift #x3F))
         (encoded (logior base
                          (ash (- 63 shift-bits) 16)  ; immr
                          (ash (- 63 shift-bits) 10)  ; imms
                          (ash rn 5)
                          rd)))
    (encode-word-le encoded)))

(defun arm64-lsr (rd rn shift)
  "LSR Xd, Xn, #shift - Logical shift right immediate"
  (let* ((base #xD3400000)
         (shift-bits (logand shift #x3F))
         (encoded (logior base
                          (ash shift-bits 16)  ; immr
                          (ash 63 10)          ; imms = 63
                          (ash rn 5)
                          rd)))
    (encode-word-le encoded)))

(defun arm64-mov (rd rm)
  "MOV Xd, Xm - Move register (via ORR)"
  (let* ((base #xAA0003E0)
         (encoded (logior base (ash rm 16) rd)))
    (encode-word-le encoded)))

(defun arm64-ldr (rt rn offset)
  (declare (ignore rt rn offset))
  ;; LDR X0, [SP] - TODO: make parametric
  (encode-word-le #xF94003E0))

(defun arm64-add-imm (rd rn imm)
  "ADD Xd, Xn, #imm12 - Add immediate"
  (let* ((base #x91000000)
         (imm12 (logand imm #xFFF))
         (encoded (logior base (ash imm12 10) (ash rn 5) rd)))
    (encode-word-le encoded)))

(defun arm64-sub-imm (rd rn imm)
  "SUB Xd, Xn, #imm12 - Subtract immediate"
  (let* ((base #xD1000000)
         (imm12 (logand imm #xFFF))
         (encoded (logior base (ash imm12 10) (ash rn 5) rd)))
    (encode-word-le encoded)))

(defun arm64-stp (rt1 rt2 rn imm)
  (declare (ignore rt1 rt2 rn imm))
  ;; STP X29, X30, [SP,#-16]!
  (encode-word-le #xA9BF7BFD))

(defun arm64-ldp (rt1 rt2 rn imm)
  (declare (ignore rt1 rt2 rn imm))
  ;; LDP X29, X30, [SP],#16
  (encode-word-le #xA8C17BFD))

(defun arm64-ret ()
  "RET - Return from subroutine"
  (encode-word-le #xD65F03C0))

(defun codegen-expr (ir runtime-addrs)
  "Enhanced codegen: literals, arithmetic, runtime calls"
  (cond
    ;; Literal: load tagged fixnum (value << 4)
    ((has-tag? ir 'lit)
     (let* ((value (cadr ir))
            (tagged (ash value 4)))  ; Tag fixnum: value << 4
       (arm64-movz 0 (logand tagged #xFFFF))))

    ;; Variable: load from stack
    ((has-tag? ir 'var)
     (let ((offset (cadr ir)))
       (arm64-ldr 0 31 (* offset 16))))

    ;; Addition: (add left right)
    ((has-tag? ir 'add)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (left-code (codegen-expr left-ir runtime-addrs))
            (right-code (codegen-expr right-ir runtime-addrs)))
       (append left-code                   ; Compute left → x0
               (arm64-mov 2 0)             ; Save left in x2
               right-code                  ; Compute right → x0
               (arm64-mov 1 0)             ; Move right to x1
               (arm64-mov 0 2)             ; Move left to x0
               (arm64-add 0 0 1))))        ; x0 = x0 + x1

    ;; Subtraction: (sub left right)
    ((has-tag? ir 'sub)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (left-code (codegen-expr left-ir runtime-addrs))
            (right-code (codegen-expr right-ir runtime-addrs)))
       (append left-code
               (arm64-mov 2 0)
               right-code
               (arm64-mov 1 0)
               (arm64-mov 0 2)
               (arm64-sub 0 0 1))))

    ;; Multiplication: (mul left right) - must untag/retag
    ((has-tag? ir 'mul)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (left-code (codegen-expr left-ir runtime-addrs))
            (right-code (codegen-expr right-ir runtime-addrs)))
       (append left-code
               (arm64-lsr 0 0 4)           ; Untag left
               (arm64-mov 2 0)             ; Save in x2
               right-code
               (arm64-lsr 0 0 4)           ; Untag right
               (arm64-mov 1 0)             ; Move to x1
               (arm64-mov 0 2)             ; Restore left
               (arm64-mul 0 0 1)           ; Multiply
               (arm64-lsl 0 0 4))))        ; Retag result

    ;; Default: zero
    (t (arm64-movz 0 0))))

(defun compile-expr (expr env fenv)
  "Enhanced IR generation: literals, arithmetic operations"
  (cond
    ;; Fixnum literal
    ((fixnum? expr)
     (list 'lit expr))

    ;; Symbol (variable)
    ((symbol? expr)
     (let ((off (env-lookup expr env)))
       (if off (list 'var off) (list 'lit 0))))

    ;; List (function call or special form)
    ((consp expr)
     (let ((op (car expr)))
       (cond
         ;; Addition
         ((eq op '+)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (list 'add
                    (compile-expr (cadr expr) env fenv)
                    (compile-expr (caddr expr) env fenv))
              (list 'lit 0)))

         ;; Subtraction
         ((eq op '-)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (list 'sub
                    (compile-expr (cadr expr) env fenv)
                    (compile-expr (caddr expr) env fenv))
              (list 'lit 0)))

         ;; Multiplication
         ((eq op '*)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (list 'mul
                    (compile-expr (cadr expr) env fenv)
                    (compile-expr (caddr expr) env fenv))
              (list 'lit 0)))

         ;; Unknown operation
         (t (list 'lit 0)))))

    ;; Unknown
    (t (list 'lit 0))))

(defun codegen-main-with-runtime (ir runtime-addrs)
  (let ((body (codegen-expr ir runtime-addrs)))
    (append (arm64-stp 29 30 31 -16)
            body
            (arm64-ldp 29 30 31 16)
            (arm64-ret))))

(defun compile-to-arm64-with-runtime (expr runtime-addrs)
  (codegen-main-with-runtime (compile-expr expr nil nil) runtime-addrs))

(defun compile-to-arm64 (expr)
  (compile-to-arm64-with-runtime expr nil))

;;; ============================================
;;; Multi-Function Compilation Stubs
;;; ============================================

(defun count-instrs (code)
  "Count number of 4-byte instructions in code list"
  (if (null code)
      0
      (+ 1 (count-instrs (nthcdr 4 code)))))

(defun compile-defun (name params body env fenv)
  "Stub: compile defun into (name param-count body-ir)"
  (declare (ignore env fenv))
  (list name (length params) (compile-expr body nil nil)))

(defun compile-forms-helper (forms env fenv)
  "Stub: compile list of forms, separating defuns from main expression
   Returns: (list-of-compiled-functions main-expression-ir)"
  (if (consp forms)
      (let ((form (car forms)))
        (if (and (consp form) (eq (car form) 'defun))
            ;; It's a defun
            (let* ((name (cadr form))
                   (params (caddr form))
                   (body (cadddr form))
                   (compiled-fn (compile-defun name params body env fenv))
                   (new-fenv (cons compiled-fn fenv))
                   (rest-result (compile-forms-helper (cdr forms) env new-fenv))
                   (rest-fns (car rest-result))
                   (main-ir (cadr rest-result)))
              (list (cons compiled-fn rest-fns) main-ir))
            ;; Not a defun - treat as main expression
            (list fenv (compile-expr form env fenv))))
      ;; No more forms
      (list fenv '(lit 0))))

(defun compile-forms (forms)
  "Stub: compile list of top-level forms"
  (compile-forms-helper forms nil nil))

(defun codegen-function-with-params (param-count body-ir runtime-addrs)
  "Stub: generate code for function with parameters
   Returns dummy prologue + body + epilogue"
  (declare (ignore param-count))
  (let ((body (codegen-expr body-ir runtime-addrs)))
    (append (arm64-stp 29 30 31 -16)
            body
            (arm64-ldp 29 30 31 16)
            (arm64-ret))))

(defun codegen-functions-helper (compiled-fns current-offset runtime-addrs)
  "Stub: generate code for all compiled functions
   Returns: (total-code function-offsets)"
  (if (consp compiled-fns)
      (let* ((fn (car compiled-fns))
             (name (car fn))
             (param-count (cadr fn))
             (body-ir (caddr fn))
             (fn-code (codegen-function-with-params param-count body-ir runtime-addrs))
             (fn-size (count-instrs fn-code))
             (rest-result (codegen-functions-helper (cdr compiled-fns)
                                                    (+ current-offset fn-size)
                                                    runtime-addrs))
             (rest-code (car rest-result))
             (rest-offsets (cadr rest-result)))
        (list (append fn-code rest-code)
              (cons (list name current-offset) rest-offsets)))
      ;; No more functions
      (list nil nil)))

(defun codegen-main-with-runtime-and-fns (ir runtime-addrs fn-offsets current-offset)
  "Stub: generate main code with function offsets (ignored in stub)"
  (declare (ignore fn-offsets current-offset))
  (codegen-main-with-runtime ir runtime-addrs))

(defun compile-program-with-functions-with-runtime (forms runtime-addrs)
  "Stub: compile entire program with function definitions
   Returns: complete machine code with all functions + main"
  (let* ((compile-result (compile-forms forms))
         (compiled-fns (car compile-result))
         (main-ir (cadr compile-result))
         (fns-result (codegen-functions-helper compiled-fns 0 runtime-addrs))
         (fns-code (car fns-result))
         (fn-offsets (cadr fns-result))
         (fns-size (count-instrs fns-code))
         (main-code (codegen-main-with-runtime-and-fns main-ir runtime-addrs fn-offsets fns-size)))
    (append fns-code main-code)))

(defun compile-program-with-functions (forms)
  "Stub: compile program using default runtime addresses"
  (compile-program-with-functions-with-runtime forms nil))
