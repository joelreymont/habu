;;;; ARM64 Assembly Code Generator
;;;; Generates .s files that can be assembled with clang

;;; ============================================
;;; Assembly Code Generation Helpers
;;; ============================================

(defun asm-header ()
  (quote (".section __TEXT,__text,regular,pure_instructions"
          ".globl _main"
          ".p2align 2"
          ""
          "_main:")))

(defun asm-footer ()
  (quote ("    ret")))

(defun asm-mov-imm (reg value)
  ;;; Generate: mov reg, #value
  (list (quote "    mov ") reg (quote ", #") value))

(defun asm-lsr (reg shift)
  ;;; Generate: lsr reg, reg, #shift
  (list (quote "    lsr ") reg (quote ", ") reg (quote ", #") shift))

(defun asm-add (dest src1 src2)
  ;;; Generate: add dest, src1, src2
  (list (quote "    add ") dest (quote ", ") src1 (quote ", ") src2))

(defun asm-sub (dest src1 src2)
  ;;; Generate: sub dest, src1, src2
  (list (quote "    sub ") dest (quote ", ") src1 (quote ", ") src2))

(defun asm-mul (dest src1 src2)
  ;;; Generate: mul dest, src1, src2
  (list (quote "    mul ") dest (quote ", ") src1 (quote ", ") src2))

(defun asm-comment (text)
  (list (quote "; ") text))

;;; ============================================
;;; Code Generation from IR
;;; ============================================

(defun has-tag? (ir tag)
  (if (cons? ir)
    (if (symbol? (car ir))
      (symbol=? (car ir) tag)
      (quote nil))
    (quote nil)))

(defun codegen-asm (ir)
  ;;; Generate ARM64 assembly from IR
  ;;; Returns list of assembly instruction lines
  (if (has-tag? ir (quote lit))
    ;;; Literal value
    (let ((value (* (car (cdr ir)) 16)))
      (list (asm-comment "Load tagged fixnum")
            (asm-mov-imm (quote "x0") value)))

    (if (has-tag? ir (quote call))
      ;;; Function call (arithmetic)
      (let ((op (car (cdr ir))))
        (let ((arg1-ir (car (cdr (cdr ir)))))
          (let ((arg2-ir (car (cdr (cdr (cdr ir))))))
            ;;; For now, only handle literal arguments
            (if (has-tag? arg1-ir (quote lit))
              (if (has-tag? arg2-ir (quote lit))
                (let ((val1 (* (car (cdr arg1-ir)) 16)))
                  (let ((val2 (* (car (cdr arg2-ir)) 16)))
                    (list
                      (asm-comment "Arithmetic operation")
                      (asm-mov-imm (quote "x1") val1)
                      (asm-mov-imm (quote "x2") val2)
                      (if (symbol=? op (quote +))
                        (asm-add (quote "x0") (quote "x1") (quote "x2"))
                        (if (symbol=? op (quote -))
                          (asm-sub (quote "x0") (quote "x1") (quote "x2"))
                          (if (symbol=? op (quote *))
                            (asm-mul (quote "x0") (quote "x1") (quote "x2"))
                            (asm-mov-imm (quote "x0") 0)))))))
                (list (asm-comment "Complex arg2")))
              (list (asm-comment "Complex arg1"))))))

      ;;; Unknown
      (list (asm-mov-imm (quote "x0") 0)))))

(defun compile-expr (expr)
  ;;; Compile Habu expression to IR
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

(defun compile-to-asm (expr)
  ;;; Full pipeline: Habu → IR → ARM64 assembly
  (let ((ir (compile-expr expr)))
    (let ((code (codegen-asm ir)))
      (let ((header (asm-header)))
        (let ((footer (asm-footer)))
          ;;; Would concatenate all parts here
          ;;; For now, just return IR for testing
          ir)))))

;;; ============================================
;;; Tests
;;; ============================================

(compile-expr 42)
(compile-expr (quote (+ 3 4)))
(compile-to-asm 42)
(compile-to-asm (quote (+ 5 6)))
