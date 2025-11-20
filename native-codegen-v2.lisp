;;;; Native Machine Code Generator for Habu - Version 2
;;;; Generates x86_64 machine code directly from IR
;;;; Uses only Habu primitives (no expt)

;;; ============================================
;;; Byte Manipulation Primitives
;;; ============================================

(defun power-256 (n)
  ;;; Calculate 256^n using repeated multiplication
  (if (= n 0)
    1
    (* 256 (power-256 (- n 1)))))

(defun int-to-byte (n offset)
  ;;; Extract byte at offset from integer
  ;;; Example: (int-to-byte 0x1234 0) → 0x34 (52)
  ;;; Example: (int-to-byte 0x1234 1) → 0x12 (18)
  (mod (/ n (power-256 offset)) 256))

(defun int-to-bytes-1 (n)
  ;;; Convert integer to 1-byte list
  (cons (int-to-byte n 0) (quote nil)))

(defun int-to-bytes-2 (n)
  ;;; Convert integer to 2-byte little-endian list
  (cons (int-to-byte n 0)
        (cons (int-to-byte n 1) (quote nil))))

(defun int-to-bytes-4 (n)
  ;;; Convert integer to 4-byte little-endian list
  (cons (int-to-byte n 0)
    (cons (int-to-byte n 1)
      (cons (int-to-byte n 2)
        (cons (int-to-byte n 3) (quote nil))))))

(defun int-to-bytes-8 (n)
  ;;; Convert integer to 8-byte little-endian list
  (cons (int-to-byte n 0)
    (cons (int-to-byte n 1)
      (cons (int-to-byte n 2)
        (cons (int-to-byte n 3)
          (cons (int-to-byte n 4)
            (cons (int-to-byte n 5)
              (cons (int-to-byte n 6)
                (cons (int-to-byte n 7) (quote nil))))))))))

(defun append-bytes (list1 list2)
  ;;; Append two byte lists
  (if (nil? list1)
    list2
    (cons (car list1) (append-bytes (cdr list1) list2))))

;;; ============================================
;;; x86_64 Code Emitters
;;; ============================================

(defun emit-x86-fixnum (n)
  ;;; Generate x86_64 code for fixnum literal
  ;;; Instruction: mov rax, immediate
  ;;; Bytes: 48 B8 [8-byte immediate]
  (let ((tagged-value (* n 16)))
    (append-bytes (cons 72 (cons 184 (quote nil)))
                  (int-to-bytes-8 tagged-value))))

(defun emit-x86-return ()
  ;;; Generate x86_64 ret instruction
  (cons 195 (quote nil)))  ; 0xC3 = ret

;;; ============================================
;;; IR to Machine Code
;;; ============================================

(defun has-tag? (ir tag)
  ;;; Check if IR node has specific tag
  (if (cons? ir)
    (if (symbol? (car ir))
      (symbol=? (car ir) tag)
      (quote nil))
    (quote nil)))

(defun emit-x86 (ir)
  ;;; Main code generator - IR to x86_64 bytes
  (if (has-tag? ir (quote lit))
    ;;; (lit N) → mov rax, N ; ret
    (append-bytes (emit-x86-fixnum (car (cdr ir)))
                  (emit-x86-return))
    ;;; Unknown - just return
    (emit-x86-return)))

;;; ============================================
;;; Compiler Integration
;;; ============================================

(defun compile-expr (expr)
  ;;; Compile Habu expression to IR
  (if (fixnum? expr)
    (list (quote lit) expr)
    (if (symbol? expr)
      (list (quote var) expr)
      expr)))

(defun compile-to-bytes (expr)
  ;;; Full pipeline: Habu → IR → x86_64 bytes
  (let ((ir (compile-expr expr)))
    (emit-x86 ir)))

;;; ============================================
;;; Test Suite
;;; ============================================

(defun test1 ()
  ;;; Test: Extract byte 0 from 0x123
  (int-to-byte 291 0))  ; Should be 35 (0x23)

(defun test2 ()
  ;;; Test: Extract byte 1 from 0x123
  (int-to-byte 291 1))  ; Should be 1

(defun test3 ()
  ;;; Test: Convert 42 to 8 bytes
  (int-to-bytes-8 42))

(defun test4 ()
  ;;; Test: Emit code for literal 42
  (emit-x86-fixnum 42))

(defun test5 ()
  ;;; Test: Full compilation of 42
  (compile-to-bytes 42))

;;; Run tests
(test1)
(test2)
(test3)
(test4)
(test5)
