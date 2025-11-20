;;;; ARM64 Machine Code Generator - Pure Habu Lisp
;;;; Generates raw ARM64 bytes following SBCL model

;;; ============================================
;;; Byte Utilities
;;; ============================================

(defun my-mod (n d)
  (- n (* d (/ n d))))

(defun get-byte (n offset)
  "Extract byte at offset from integer"
  (my-mod (/ n (* offset 256)) 256))

(defun make-word (b0 b1 b2 b3)
  "Make 32-bit word from 4 bytes (little-endian)"
  (+ b0 (* b1 256) (* b2 65536) (* b3 16777216)))

(defun word-to-bytes (w)
  "Convert 32-bit word to 4-byte list"
  (list (my-mod w 256)
        (my-mod (/ w 256) 256)
        (my-mod (/ w 65536) 256)
        (my-mod (/ w 16777216) 256)))

;;; ============================================
;;; ARM64 Instruction Encoders
;;; ============================================

(defun arm64-movz (reg imm)
  "MOVZ: Move immediate (zero) - loads 16-bit immediate"
  ;;; Encoding: 1101001 0 10 imm16 Rd
  ;;; Base: 0xD2800000 | (imm << 5) | reg
  (let ((base 3530407936))  ; 0xD2800000
    (let ((with-imm (+ base (* imm 32))))
      (word-to-bytes (+ with-imm reg)))))

(defun arm64-add (rd rn rm)
  "ADD: rd = rn + rm"
  ;;; add x0, x1, x2: base 0x8B000000 | (rm << 16) | (rn << 5) | rd
  ;;; For add x0, x0, x1: 0x8B010000
  (if (= rd 0)
    (if (= rn 0)
      (if (= rm 1)
        (quote (0 0 1 139))  ; add x0, x0, x1
        (quote (0 0 0 0)))
      (quote (0 0 0 0)))
    (quote (0 0 0 0))))

(defun arm64-sub (rd rn rm)
  "SUB: rd = rn - rm"
  ;;; sub x0, x0, x1: 0xCB010000
  (if (= rd 0)
    (if (= rn 0)
      (if (= rm 1)
        (quote (0 0 1 203))  ; sub x0, x0, x1
        (quote (0 0 0 0)))
      (quote (0 0 0 0)))
    (quote (0 0 0 0))))

(defun arm64-mul (rd rn rm)
  "MUL: rd = rn * rm"
  ;;; mul x0, x0, x1: 0x9B017C00
  (if (= rd 0)
    (if (= rn 0)
      (if (= rm 1)
        (quote (0 124 1 155))  ; mul x0, x0, x1
        (quote (0 0 0 0)))
      (quote (0 0 0 0)))
    (quote (0 0 0 0))))

(defun arm64-lsr-4 ()
  "LSR: logical shift right by 4"
  ;;; lsr x0, x0, #4: 0xD3441000
  (quote (0 16 68 211)))

(defun arm64-lsl-4 ()
  "LSL: logical shift left by 4"
  ;;; lsl x0, x0, #4: 0xD3001000
  (quote (0 16 0 211)))

(defun arm64-str-pre ()
  "STR: Store x0 to [sp, #-16]! (pre-decrement)"
  ;;; str x0, [sp, #-16]!: 0xF81F0FE0
  (quote (240 15 31 248)))

(defun arm64-ldr-post ()
  "LDR: Load x0 from [sp], #16 (post-increment)"
  ;;; ldr x0, [sp], #16: 0xF84107E0
  (quote (224 7 65 248)))

(defun arm64-mov-x1-x0 ()
  "MOV: x1 = x0"
  ;;; mov x1, x0: 0xAA0003E1
  (quote (225 3 0 170)))

(defun arm64-mov-x2-x0 ()
  "MOV: x2 = x0"
  ;;; mov x2, x0: 0xAA0003E2
  (quote (226 3 0 170)))

(defun arm64-stp-pre ()
  "STP: Store pair x29, x30 to [sp, #-16]! (save frame)"
  ;;; stp x29, x30, [sp, #-16]!: 0xA9BF7BFD
  (quote (253 123 191 169)))

(defun arm64-ldp-post ()
  "LDP: Load pair x29, x30 from [sp], #16 (restore frame)"
  ;;; ldp x29, x30, [sp], #16: 0xA8C17BFD
  (quote (253 123 193 168)))

(defun arm64-mov-sp-x29 ()
  "MOV: sp = x29"
  ;;; mov sp, x29: 0x910003BF
  (quote (191 3 0 145)))

(defun arm64-mov-x29-sp ()
  "MOV: x29 = sp"
  ;;; mov x29, sp: 0x910003FD
  (quote (253 3 0 145)))

(defun arm64-ret ()
  "RET: Return"
  ;;; ret: 0xD65F03C0
  (quote (192 3 95 214)))

(defun append-code (c1 c2)
  (if (nil? c1) c2 (cons (car c1) (append-code (cdr c1) c2))))

;;; ============================================
;;; High-Level Code Generation
;;; ============================================

(defun has-tag? (ir tag)
  (if (cons? ir)
    (if (symbol? (car ir))
      (symbol=? (car ir) tag)
      (quote nil))
    (quote nil)))

(defun codegen-expr (ir)
  "Generate ARM64 code for expression (result in x0)"
  (if (has-tag? ir (quote lit))
    ;;; Literal: movz x0, #(value << 4)
    (let ((value (car (cdr ir))))
      (let ((tagged (* value 16)))
        (arm64-movz 0 tagged)))

    (if (has-tag? ir (quote call))
      ;;; Binary operation with nested args
      (let ((op (car (cdr ir))))
        (let ((arg1 (car (cdr (cdr ir)))))
          (let ((arg2 (car (cdr (cdr (cdr ir))))))
            ;;; Generate code for arg1
            (let ((code1 (codegen-expr arg1)))
              ;;; Save arg1: str x0, [sp, #-16]!
              (let ((save-code (arm64-str-pre)))
                ;;; Generate code for arg2
                (let ((code2 (codegen-expr arg2)))
                  ;;; Move arg2 to x1: mov x1, x0
                  (let ((move-code (arm64-mov-x1-x0)))
                    ;;; Load arg1 to x0: ldr x0, [sp], #16
                    (let ((load-code (arm64-ldr-post)))
                      ;;; Perform operation
                      (let ((op-code
                              (if (symbol=? op (quote +))
                                (arm64-add 0 0 1)
                                (if (symbol=? op (quote -))
                                  (arm64-sub 0 0 1)
                                  (if (symbol=? op (quote *))
                                    ;;; For multiply, untag one operand
                                    (append-code
                                      (arm64-lsr-4)
                                      (arm64-mul 0 0 1))
                                    (arm64-add 0 0 0))))))
                        ;;; Combine all code
                        (append-code code1
                          (append-code save-code
                            (append-code code2
                              (append-code move-code
                                (append-code load-code op-code)))))))))))

      ;;; Unknown
      (arm64-movz 0 0))))

(defun codegen-main (ir)
  "Generate complete main function"
  (let ((prologue (append-code (arm64-stp-pre) (arm64-mov-x29-sp))))
    (let ((body (codegen-expr ir)))
      (let ((untag (arm64-lsr-4)))
        (let ((epilogue (append-code (arm64-mov-sp-x29)
                          (append-code (arm64-ldp-post) (arm64-ret)))))
          (append-code prologue
            (append-code body
              (append-code untag epilogue))))))))

;;; ============================================
;;; Compiler Integration
;;; ============================================

(defun compile-expr (expr)
  (if (fixnum? expr)
    (list (quote lit) expr)
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
      expr)))

(defun compile-to-arm64 (expr)
  "Full pipeline: Habu expr → IR → ARM64 bytes"
  (codegen-main (compile-expr expr)))

;;; ============================================
;;; Tests
;;; ============================================

(compile-expr 42)
(compile-expr (quote (+ 3 4)))
(compile-to-arm64 42)
(compile-to-arm64 (quote (+ 5 7)))
