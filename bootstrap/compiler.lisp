;;;; Habu Bootstrap Compiler
;;;; Compiles Habu Lisp to native x86_64 and ARM64 machine code

(defpackage :habu-compiler
  (:use :cl)
  (:export #:compile-expression
           #:compile-to-binary
           #:*target-arch*))

(in-package :habu-compiler)

;;; Target architecture (x86_64 or arm64)
(defvar *target-arch* :x86_64)

;;; Compiler intermediate representation
(defstruct expr
  type
  value
  args)

;;; Parse Lisp expression to IR
(defun parse (form)
  "Parse a Lisp form into compiler IR"
  (cond
    ((integerp form)
     (make-expr :type 'fixnum :value form))

    ((symbolp form)
     (make-expr :type 'variable :value form))

    ((and (consp form) (symbolp (first form)))
     (let ((op (first form))
           (args (rest form)))
       (make-expr :type 'call
                  :value op
                  :args (mapcar #'parse args))))

    (t
     (error "Cannot parse form: ~S" form))))

;;; Code generation for x86_64
(defun emit-x86_64 (expr)
  "Generate x86_64 machine code for expression"
  (ecase (expr-type expr)
    (fixnum
     ;; Load fixnum into RAX
     ;; mov rax, imm64
     (let ((val (* (expr-value expr) 16))) ; Tag as fixnum (shift left 4)
       (append (list #x48 #xB8)           ; REX.W + mov rax prefix
               (int-to-bytes val 8))))

    (call
     (let ((op (expr-value expr))
           (args (expr-args expr)))
       (cond
         ((eq op '+)
          ;; Compile (+ a b)
          (append (emit-x86_64 (first args))   ; Result in RAX
                  (list #x50)                   ; push rax
                  (emit-x86_64 (second args))   ; Result in RAX
                  (list #x48 #x8B #x1C #x24)    ; mov rbx, [rsp]
                  (list #x48 #x01 #xD8)         ; add rax, rbx
                  (list #x48 #x83 #xC4 #x08))) ; add rsp, 8 (pop)

         ((eq op '-)
          ;; Compile (- a b)
          (append (emit-x86_64 (first args))
                  (list #x50)
                  (emit-x86_64 (second args))
                  (list #x48 #x8B #x1C #x24)    ; mov rbx, [rsp]
                  (list #x48 #x89 #xD9)         ; mov rcx, rbx
                  (list #x48 #x29 #xC1)         ; sub rcx, rax
                  (list #x48 #x89 #xC8)         ; mov rax, rcx
                  (list #x48 #x83 #xC4 #x08)))

         (t
          (error "Unknown operator: ~S" op)))))))

;;; Code generation for ARM64
(defun emit-arm64 (expr)
  "Generate ARM64 machine code for expression"
  (ecase (expr-type expr)
    (fixnum
     ;; Load fixnum into X0
     ;; mov x0, #imm
     (let ((val (* (expr-value expr) 16))) ; Tag as fixnum
       (if (< val 65536)
           ;; Use MOVZ for small immediate
           (int-to-bytes (logior #xD2800000 ; MOVZ X0, imm16
                                 (ash (logand val #xFFFF) 5))
                         4)
           ;; Use MOVZ + MOVK for larger values
           (append (int-to-bytes (logior #xD2800000
                                         (ash (logand val #xFFFF) 5))
                                 4)
                   (int-to-bytes (logior #xF2A00000 ; MOVK X0, imm16, LSL#16
                                         (ash (logand (ash val -16) #xFFFF) 5))
                                 4)))))

    (call
     (let ((op (expr-value expr))
           (args (expr-args expr)))
       (cond
         ((eq op '+)
          ;; Compile (+ a b) for ARM64
          (append (emit-arm64 (first args))        ; Result in X0
                  (list #xFD #x7B #xBF #xA9)       ; stp x29, x30, [sp, #-16]!
                  (list #xE0 #x03 #x00 #xAA)       ; mov x0, x0 (save)
                  (emit-arm64 (second args))        ; Result in X0
                  (list #xE1 #x03 #x00 #xAA)       ; mov x1, x0
                  (list #xE0 #x03 #x01 #xAA)       ; mov x0, x1 (restore from stack would be here)
                  (list #x00 #x00 #x01 #x8B)       ; add x0, x0, x1
                  (list #xFD #x7B #xC1 #xA8)))     ; ldp x29, x30, [sp], #16

         (t
          (error "Unknown operator: ~S" op)))))))

;;; Helper: Convert integer to little-endian byte list
(defun int-to-bytes (n size)
  "Convert integer N to SIZE bytes in little-endian order"
  (loop for i from 0 below size
        collect (ldb (byte 8 (* i 8)) n)))

;;; Helper: Convert byte list to vector
(defun bytes-to-vector (bytes)
  (make-array (length bytes)
              :element-type '(unsigned-byte 8)
              :initial-contents bytes))

;;; Main compilation entry point
(defun compile-expression (form &key (arch :x86_64))
  "Compile a Lisp form to machine code for the target architecture"
  (let ((*target-arch* arch))
    (let* ((ir (parse form))
           (code (ecase arch
                   (:x86_64 (emit-x86_64 ir))
                   (:arm64 (emit-arm64 ir)))))
      (bytes-to-vector code))))

;;; Write machine code to binary file with minimal ELF wrapper
(defun compile-to-binary (form output-file &key (arch :x86_64))
  "Compile form to executable binary"
  (let* ((code (compile-expression form :arch arch))
         (code-size (length code)))
    (with-open-file (out output-file
                         :direction :output
                         :if-exists :supersede
                         :element-type '(unsigned-byte 8))
      (ecase arch
        (:x86_64
         ;; Minimal x86_64 code - just the instructions + ret
         (write-sequence code out)
         (write-byte #xC3 out)) ; ret instruction

        (:arm64
         ;; Minimal ARM64 code - just the instructions + ret
         (write-sequence code out)
         ;; ret instruction for ARM64
         (write-sequence #(#xC0 #x03 #x5F #xD6) out))))

    ;; Return info about compilation
    (values output-file code-size)))
