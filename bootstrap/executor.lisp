;;;; executor.lisp - Execute compiled Habu machine code

(require :sb-posix)
(in-package :habu-compiler)

;;; Code Execution System
;;; Phase 1: Execute within SBCL environment using sb-alien

(defvar *code-memory-blocks* (make-hash-table)
  "Registry of allocated executable memory blocks")

(defstruct code-block
  "Represents an allocated block of executable memory"
  address     ; System address pointer (SAP)
  size        ; Size in bytes
  code        ; Original bytecode (for debugging)
  name)       ; Optional name

;;; Memory Allocation

#+sbcl
(defun allocate-executable-memory (size &optional name)
  "Allocate executable memory using mmap.
   Returns a code-block structure."
  ;; Round up to page size (typically 4096 bytes)
  (let* ((page-size 4096)
         (aligned-size (* (ceiling size page-size) page-size)))

    ;; Allocate executable memory using mmap
    ;; Use sb-posix constants for portability
    (let* ((prot (logior sb-posix:prot-read
                         sb-posix:prot-write
                         sb-posix:prot-exec))
           (flags (logior sb-posix:map-private
                          sb-posix:map-anon))
           (sap (sb-posix:mmap nil aligned-size prot flags -1 0)))

      ;; Check for failure (MAP_FAILED is typically -1)
      (when (= (sb-sys:sap-int sap) (lognot 0))  ; All 1's = -1 as unsigned
        (error "Failed to allocate executable memory"))

      ;; Create code block
      (let ((block (make-code-block
                    :address sap  ; Store SAP directly
                    :size aligned-size
                    :name name)))

        ;; Register for cleanup
        (setf (gethash (sb-sys:sap-int sap) *code-memory-blocks*)
              block)

        block))))

#+sbcl
(defun free-executable-memory (block)
  "Free an allocated code block"
  (when block
    (sb-posix:munmap (code-block-address block) (code-block-size block))
    (remhash (sb-sys:sap-int (code-block-address block))
             *code-memory-blocks*)))

;;; Code Loading

#+sbcl
(defun load-code-to-memory (code-bytes &optional name)
  "Load bytecode into executable memory.
   Returns code-block structure."
  (let* ((size (length code-bytes))
         (block (allocate-executable-memory size name))
         (mem (code-block-address block)))  ; This is already a SAP

    ;; Copy bytecode to memory
    (loop for byte across code-bytes
          for i from 0
          do (setf (sb-sys:sap-ref-8 mem i) byte))

    ;; Store original code for debugging
    (setf (code-block-code block) code-bytes)

    block))

;;; Function Pointer Creation

#+sbcl
(defun make-function-pointer (code-block arity)
  "Create a callable function pointer from a code block.
   Arity specifies number of arguments (0-4 supported).

   Note: This uses sb-alien to create a callable wrapper around
   the raw machine code. The code must follow the System V AMD64 ABI."
  (let* ((mem (code-block-address code-block))  ; Already a SAP
         (addr (sb-sys:sap-int mem)))

    ;; Create wrapper function that calls the machine code
    ;; We use sb-alien:alien-funcall with a manually created alien pointer
    (ecase arity
      (0
       ;; Function with no arguments
       (lambda ()
         (let ((fn-ptr (sb-alien:sap-alien
                        (sb-sys:int-sap addr)
                        (* (function sb-alien:unsigned-long)))))
           (sb-alien:alien-funcall fn-ptr))))

      (1
       ;; Function with 1 argument
       (lambda (arg1)
         (let ((fn-ptr (sb-alien:sap-alien
                        (sb-sys:int-sap addr)
                        (* (function sb-alien:unsigned-long
                                     sb-alien:unsigned-long)))))
           (sb-alien:alien-funcall fn-ptr arg1))))

      (2
       ;; Function with 2 arguments
       (lambda (arg1 arg2)
         (let ((fn-ptr (sb-alien:sap-alien
                        (sb-sys:int-sap addr)
                        (* (function sb-alien:unsigned-long
                                     sb-alien:unsigned-long
                                     sb-alien:unsigned-long)))))
           (sb-alien:alien-funcall fn-ptr arg1 arg2))))

      (3
       ;; Function with 3 arguments
       (lambda (arg1 arg2 arg3)
         (let ((fn-ptr (sb-alien:sap-alien
                        (sb-sys:int-sap addr)
                        (* (function sb-alien:unsigned-long
                                     sb-alien:unsigned-long
                                     sb-alien:unsigned-long
                                     sb-alien:unsigned-long)))))
           (sb-alien:alien-funcall fn-ptr arg1 arg2 arg3))))

      (4
       ;; Function with 4 arguments
       (lambda (arg1 arg2 arg3 arg4)
         (let ((fn-ptr (sb-alien:sap-alien
                        (sb-sys:int-sap addr)
                        (* (function sb-alien:unsigned-long
                                     sb-alien:unsigned-long
                                     sb-alien:unsigned-long
                                     sb-alien:unsigned-long
                                     sb-alien:unsigned-long)))))
           (sb-alien:alien-funcall fn-ptr arg1 arg2 arg3 arg4)))))))

;;; High-Level Execution Interface

(defun execute-expression (expr &key (arch :x86_64))
  "Compile and execute an expression, returning the result.
   This is the main entry point for code execution."
  (initialize-runtime-integration)

  ;; Compile expression
  (let ((code (compile-expression expr :arch arch)))

    (when (zerop (length code))
      (error "Expression compiled to 0 bytes: ~S" expr))

    ;; Load into executable memory
    (let ((block (load-code-to-memory code (format nil "~S" expr))))

      (unwind-protect
          (progn
            ;; Create function pointer (0 arguments for expression)
            (let ((fn (make-function-pointer block 0)))
              ;; Call and get result (tagged fixnum)
              (funcall fn)))

        ;; Cleanup
        (free-executable-memory block)))))

(defun compile-and-call (function-name &rest args)
  "Compile a function call and execute it.
   The function must be defined in *function-table*."
  (let ((fn-def (gethash function-name *function-table*)))
    (unless fn-def
      (error "Undefined function: ~S" function-name))

    (let* ((params (car fn-def))
           (body (cdr fn-def))
           (arity (length params)))

      (unless (= arity (length args))
        (error "Wrong number of arguments for ~S: expected ~D, got ~D"
               function-name arity (length args)))

      ;; Build lambda expression with arguments
      (let ((expr `((lambda ,params ,body) ,@args)))
        (execute-expression expr)))))

;;; Utility Functions

(defun untag-fixnum (tagged-value)
  "Convert tagged fixnum to regular integer"
  (ash tagged-value -4))

(defun tag-fixnum (value)
  "Convert regular integer to tagged fixnum"
  (ash value 4))

(defun execute-and-untag (expr &key (arch :x86_64))
  "Execute expression and return untagged result"
  (untag-fixnum (execute-expression expr :arch arch)))

;;; Debugging

(defun disassemble-code-block (block)
  "Display information about a code block (for debugging)"
  (format t "Code Block: ~A~%" (code-block-name block))
  (format t "  Address: ~A~%" (code-block-address block))
  (format t "  Size: ~D bytes~%" (code-block-size block))
  (when (code-block-code block)
    (format t "  Bytecode: ~{~2,'0X ~}~%" (coerce (code-block-code block) 'list))))

(export '(allocate-executable-memory
          free-executable-memory
          load-code-to-memory
          make-function-pointer
          execute-expression
          compile-and-call
          execute-and-untag
          untag-fixnum
          tag-fixnum
          disassemble-code-block))
