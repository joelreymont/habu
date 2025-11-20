;;;; Native Machine Code Generator for Habu
;;;; Generates x86_64 machine code directly from IR
;;;; Following SBCL architecture

;;; ============================================
;;; Byte Manipulation Primitives
;;; ============================================

(defun int-to-byte (n offset)
  ;;; Extract byte at offset from integer
  ;;; Example: (int-to-byte 0x1234 0) → 0x34
  ;;; Example: (int-to-byte 0x1234 1) → 0x12
  (mod (/ n (expt 256 offset)) 256))

(defun int-to-bytes-helper (n size offset acc)
  ;;; Helper for int-to-bytes - builds byte list recursively
  (if (= offset size)
    acc
    (int-to-bytes-helper n size (+ offset 1)
                        (cons (int-to-byte n offset) acc))))

(defun int-to-bytes (n size)
  ;;; Convert integer to little-endian byte list
  ;;; Example: (int-to-bytes 0x1234 2) → (0x34 0x12)
  (reverse (int-to-bytes-helper n size 0 (quote nil))))

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
  (let ((tagged-value (* n 16)))  ; Tag as fixnum (shift left 4)
    (append-bytes (quote (72 184))  ; 0x48 0xB8 = REX.W + mov rax
                  (int-to-bytes tagged-value 8))))

(defun emit-x86-variable (offset)
  ;;; Generate x86_64 code for variable lookup
  ;;; Instruction: mov rax, [rsp + offset]
  (if (= offset 0)
    ;;; mov rax, [rsp]
    (quote (72 139 4 36))  ; 0x48 0x8B 0x04 0x24
    ;;; mov rax, [rsp + disp32]
    (append-bytes (quote (72 139 132 36))  ; 0x48 0x8B 0x84 0x24
                  (int-to-bytes offset 4))))

(defun emit-x86-add (arg1-code arg2-code)
  ;;; Generate x86_64 code for addition
  ;;; Strategy:
  ;;;   - Evaluate arg1 → RAX
  ;;;   - Push RAX to stack
  ;;;   - Evaluate arg2 → RAX
  ;;;   - Pop stack to RBX
  ;;;   - Add RAX and RBX → RAX
  (append-bytes arg1-code
    (append-bytes (quote (80))  ; push rax
      (append-bytes arg2-code
        (append-bytes (quote (91))  ; pop rbx
          (quote (72 1 216)))))))  ; add rax, rbx (0x48 0x01 0xD8)

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
    ;;; (lit N) → mov rax, N
    (emit-x86-fixnum (car (cdr ir)))

    (if (has-tag? ir (quote var))
      ;;; (var SYM) → mov rax, [rsp + offset]
      ;;; TODO: need environment to get offset
      (emit-x86-variable 0)

      (if (has-tag? ir (quote call))
        ;;; (call OP ARG1 ARG2) → emit operation
        (let ((op (car (cdr ir))))
          (let ((arg1 (car (cdr (cdr ir)))))
            (let ((arg2 (car (cdr (cdr (cdr ir))))))
              (if (symbol=? op (quote +))
                (emit-x86-add (emit-x86 arg1) (emit-x86 arg2))
                ;;; TODO: other operators
                (quote (144))))))  ; nop

        ;;; Unknown IR node
        (quote (144))))))  ; nop

;;; ============================================
;;; Test Suite
;;; ============================================

(defun test-byte-conversion ()
  ;;; Test byte manipulation
  (progn
    (int-to-byte 291 0)      ; Should be 35 (0x23)
    (int-to-byte 291 1)      ; Should be 1
    (int-to-bytes 42 1)      ; Should be (42)
    (int-to-bytes 258 2)))   ; Should be (2 1) in little-endian

(defun test-emit-fixnum ()
  ;;; Test fixnum emission
  (emit-x86-fixnum 42))

(defun test-emit-add ()
  ;;; Test addition emission
  (let ((ir (quote (call + (lit 1) (lit 2)))))
    (emit-x86 ir)))

;;; Run tests
(test-byte-conversion)
(test-emit-fixnum)
(test-emit-add)
