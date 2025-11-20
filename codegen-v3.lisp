;;;; x86_64 Code Generator - Version 3
;;;; Using iterative list building to avoid stack overflow

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
;;; List Building Helpers
;;; ============================================

(defun build-list-2 (a b)
  (cons a (cons b (quote nil))))

(defun build-list-4 (a b c d)
  (cons a (cons b (cons c (cons d (quote nil))))))

(defun build-list-8 (a b c d e f g h)
  ;;; Build 8-element list iteratively
  (let ((l1 (build-list-4 e f g h)))
    (let ((l2 (build-list-4 a b c d)))
      (let ((result (cons (car l2) (cons (car (cdr l2)) (cons (car (cdr (cdr l2))) (cons (car (cdr (cdr (cdr l2)))) l1))))))
        result))))

;;; ============================================
;;; Code Emitters
;;; ============================================

(defun emit-mov-rax-imm64 (value)
  ;;; Emit: mov rax, immediate64
  ;;; Opcode: 48 B8 [8 bytes]
  (let ((b0 (int-to-byte value 0)))
    (let ((b1 (int-to-byte value 1)))
      (let ((b2 (int-to-byte value 2)))
        (let ((b3 (int-to-byte value 3)))
          (let ((b4 (int-to-byte value 4)))
            (let ((b5 (int-to-byte value 5)))
              (let ((b6 (int-to-byte value 6)))
                (let ((b7 (int-to-byte value 7)))
                  (let ((bytes (build-list-8 b0 b1 b2 b3 b4 b5 b6 b7)))
                    (cons 72 (cons 184 bytes))))))))))))

(defun emit-ret ()
  (cons 195 (quote nil)))

(defun append-code (c1 c2)
  (if (nil? c1) c2
    (cons (car c1) (append-code (cdr c1) c2))))

;;; ============================================
;;; Tests
;;; ============================================

;;; Test list building
(build-list-2 1 2)
(build-list-4 1 2 3 4)
(build-list-8 1 2 3 4 5 6 7 8)

;;; Test mov emission
(emit-mov-rax-imm64 672)

;;; Test complete instruction
(append-code (emit-mov-rax-imm64 672) (emit-ret))
