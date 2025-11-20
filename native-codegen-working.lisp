;;;; Native Machine Code Generator for Habu - Working Version
;;;; Generates x86_64 machine code directly from IR

;;; ============================================
;;; Math Helpers
;;; ============================================

(defun my-mod (n d)
  ;;; Modulo operation: n mod d
  (- n (* d (/ n d))))

(defun power-256 (n)
  ;;; Calculate 256^n
  (if (= n 0)
    1
    (* 256 (power-256 (- n 1)))))

;;; ============================================
;;; Byte Manipulation
;;; ============================================

(defun int-to-byte (n offset)
  ;;; Extract byte at offset from integer (little-endian)
  (my-mod (/ n (power-256 offset)) 256))

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
  ;;; Emit: mov rax, immediate (tagged fixnum)
  ;;; Opcode: 48 B8 [8-byte value]
  (let ((tagged (* n 16)))
    (append-bytes (cons 72 (cons 184 (quote nil)))
                  (int-to-bytes-8 tagged))))

(defun emit-x86-ret ()
  ;;; Emit: ret
  ;;; Opcode: C3
  (cons 195 (quote nil)))

;;; ============================================
;;; IR Detection
;;; ============================================

(defun has-tag? (ir tag)
  (if (cons? ir)
    (if (symbol? (car ir))
      (symbol=? (car ir) tag)
      (quote nil))
    (quote nil)))

;;; ============================================
;;; Code Generation
;;; ============================================

(defun emit-x86 (ir)
  ;;; Generate x86_64 bytes from IR
  (if (has-tag? ir (quote lit))
    (append-bytes (emit-x86-fixnum (car (cdr ir)))
                  (emit-x86-ret))
    (emit-x86-ret)))

;;; ============================================
;;; Compiler (from earlier)
;;; ============================================

(defun compile-expr (expr)
  (if (fixnum? expr)
    (list (quote lit) expr)
    (if (symbol? expr)
      (list (quote var) expr)
      expr)))

(defun compile-to-machine-code (expr)
  ;;; Full pipeline: Habu expr → IR → x86_64 bytes
  (emit-x86 (compile-expr expr)))

;;; ============================================
;;; Tests
;;; ============================================

;;; Test byte extraction
(int-to-byte 291 0)      ; 35
(int-to-byte 291 1)      ; 1

;;; Test byte conversion
(int-to-bytes-8 42)      ; (42 0 0 0 0 0 0 0)

;;; Test code generation for literal 42
(emit-x86-fixnum 42)     ; Should be mov rax, 672 (42 * 16)

;;; Test full compilation
(compile-to-machine-code 42)  ; Full machine code with ret
