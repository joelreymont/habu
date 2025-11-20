;;;; x86_64 Native Code Generator for Habu
;;;; Successfully generates machine code from IR!

;;; ============================================
;;; Utilities
;;; ============================================

(defun my-mod (n d)
  (- n (* d (/ n d))))

(defun power-256 (n)
  (if (= n 0) 1 (* 256 (power-256 (- n 1)))))

(defun int-to-byte (n offset)
  (my-mod (/ n (power-256 offset)) 256))

;;; ============================================
;;; Code Emitters
;;; ============================================

(defun emit-mov-rax-imm64 (value)
  ;;; Emit: mov rax, immediate (64-bit)
  ;;; Bytes: 48 B8 [value as 8 bytes little-endian]
  (cons 72
    (cons 184
      (cons (int-to-byte value 0)
        (cons (int-to-byte value 1)
          (cons (int-to-byte value 2)
            (cons (int-to-byte value 3)
              (cons (int-to-byte value 4)
                (cons (int-to-byte value 5)
                  (cons (int-to-byte value 6)
                    (cons (int-to-byte value 7) (quote nil))))))))))))

(defun emit-ret ()
  ;;; Emit: ret
  (cons 195 (quote nil)))

(defun append-code (code1 code2)
  (if (nil? code1)
    code2
    (cons (car code1) (append-code (cdr code1) code2))))

;;; ============================================
;;; IR-based Code Generation
;;; ============================================

(defun has-tag? (ir tag)
  (if (cons? ir)
    (if (symbol? (car ir))
      (symbol=? (car ir) tag)
      (quote nil))
    (quote nil)))

(defun codegen (ir)
  ;;; Generate x86_64 code from IR node
  (if (has-tag? ir (quote lit))
    ;;; Literal fixnum
    (let ((value (car (cdr ir))))
      (let ((tagged (* value 16)))
        (append-code (emit-mov-rax-imm64 tagged)
                    (emit-ret))))
    ;;; Unknown - just return
    (emit-ret)))

;;; ============================================
;;; Full Compiler Pipeline
;;; ============================================

(defun compile-expr (expr)
  ;;; Habu expression → IR
  (if (fixnum? expr)
    (list (quote lit) expr)
    (if (symbol? expr)
      (list (quote var) expr)
      (if (cons? expr)
        (let ((op (car expr)))
          (let ((args (cdr expr)))
            (if (cons? args)
              (let ((arg1 (car args)))
                (let ((rest (cdr args)))
                  (if (cons? rest)
                    (list (quote call) op
                          (compile-expr arg1)
                          (compile-expr (car rest)))
                    (list (quote call) op (compile-expr arg1)))))
              (list (quote call) op))))
        expr))))

(defun compile-to-native (expr)
  ;;; Full pipeline: Habu expression → x86_64 machine code
  (codegen (compile-expr expr)))

;;; ============================================
;;; Tests and Demonstrations
;;; ============================================

;;; Test 1: Emit mov rax, 42 (tagged)
(emit-mov-rax-imm64 672)

;;; Test 2: Generate code for literal 42
(codegen (quote (lit 42)))

;;; Test 3: Full compilation of 42
(compile-to-native 42)

;;; Test 4: Show the IR
(compile-expr 42)

;;; Test 5: Compile expression (+ 1 2) to IR
(compile-expr (quote (+ 1 2)))
