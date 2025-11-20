;;;; Complete x86_64 Code Generator
;;;; Arithmetic operations with 4-byte immediates

;;; ============================================
;;; Utilities
;;; ============================================

(defun my-mod (n d)
  (- n (* d (/ n d))))

(defun power-256 (n)
  (if (= n 0) 1 (* 256 (power-256 (- n 1)))))

(defun int-to-byte (n offset)
  (my-mod (/ n (power-256 offset)) 256))

(defun append-code (c1 c2)
  (if (nil? c1) c2 (cons (car c1) (append-code (cdr c1) c2))))

;;; ============================================
;;; x86_64 Instruction Emitters
;;; ============================================

(defun emit-mov-eax-imm32 (value)
  ;;; mov eax, immediate32
  ;;; Opcode: B8 [4 bytes]
  (cons 184
    (cons (int-to-byte value 0)
      (cons (int-to-byte value 1)
        (cons (int-to-byte value 2)
          (cons (int-to-byte value 3) (quote nil)))))))

(defun emit-push-rax ()
  ;;; push rax
  ;;; Opcode: 50
  (cons 80 (quote nil)))

(defun emit-pop-rbx ()
  ;;; pop rbx
  ;;; Opcode: 5B
  (cons 91 (quote nil)))

(defun emit-add-eax-ebx ()
  ;;; add eax, ebx
  ;;; Opcode: 01 D8
  (cons 1 (cons 216 (quote nil))))

(defun emit-sub-eax-ebx ()
  ;;; sub eax, ebx
  ;;; Opcode: 29 D8
  (cons 41 (cons 216 (quote nil))))

(defun emit-imul-ebx ()
  ;;; imul ebx (signed multiply eax * ebx → eax)
  ;;; Opcode: 0F AF C3
  (cons 15 (cons 175 (cons 195 (quote nil)))))

(defun emit-ret ()
  ;;; ret
  ;;; Opcode: C3
  (cons 195 (quote nil)))

;;; ============================================
;;; High-Level Code Generation
;;; ============================================

(defun has-tag? (ir tag)
  (if (cons? ir)
    (if (symbol? (car ir))
      (symbol=? (car ir) tag)
      (quote nil))
    (quote nil)))

(defun codegen (ir)
  ;;; Generate x86_64 code from IR
  (if (has-tag? ir (quote lit))
    ;;; Literal: mov eax, value; ret
    (let ((value (* (car (cdr ir)) 16)))
      (append-code (emit-mov-eax-imm32 value) (emit-ret)))

    (if (has-tag? ir (quote call))
      ;;; Function call
      (let ((op (car (cdr ir))))
        (let ((arg1-ir (car (cdr (cdr ir)))))
          (let ((arg2-ir (car (cdr (cdr (cdr ir))))))
            ;;; Emit code for arg1 → eax
            (let ((arg1-code (codegen-expr arg1-ir)))
              ;;; Push result
              (let ((push-code (emit-push-rax)))
                ;;; Emit code for arg2 → eax
                (let ((arg2-code (codegen-expr arg2-ir)))
                  ;;; Pop arg1 → ebx
                  (let ((pop-code (emit-pop-rbx)))
                    ;;; Perform operation
                    (let ((op-code
                            (if (symbol=? op (quote +))
                              (emit-add-eax-ebx)
                              (if (symbol=? op (quote -))
                                (emit-sub-eax-ebx)
                                (if (symbol=? op (quote *))
                                  (emit-imul-ebx)
                                  (emit-ret))))))
                      ;;; Combine all pieces
                      (append-code arg1-code
                        (append-code push-code
                          (append-code arg2-code
                            (append-code pop-code
                              (append-code op-code (emit-ret))))))))))))

      ;;; Unknown
      (emit-ret))))

(defun codegen-expr (ir)
  ;;; Generate code without ret (for intermediate values)
  (if (has-tag? ir (quote lit))
    (let ((value (* (car (cdr ir)) 16)))
      (emit-mov-eax-imm32 value))
    (emit-mov-eax-imm32 0)))

;;; ============================================
;;; Compiler Integration
;;; ============================================

(defun compile-expr (expr)
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

(defun compile-to-machine-code (expr)
  ;;; Full pipeline: Habu → IR → x86_64 bytes
  (codegen (compile-expr expr)))

;;; ============================================
;;; Tests
;;; ============================================

;;; Test emit instructions
(emit-mov-eax-imm32 672)
(emit-push-rax)
(emit-add-eax-ebx)
(emit-ret)

;;; Test compilation
(compile-expr 42)
(compile-expr (quote (+ 1 2)))

;;; Test code generation
(codegen (quote (lit 42)))
(compile-to-machine-code 42)
(compile-to-machine-code (quote (+ 3 4)))
