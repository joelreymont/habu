;;;; JIT Execute - Runtime JIT code execution
;;;;
;;;; This module provides the runtime environment for executing
;;;; JIT-compiled code. It handles:
;;;;   - Memory allocation (mmap with MAP_JIT)
;;;;   - Code loading and cache management
;;;;   - Function execution
;;;;
;;;; The REPL uses this to execute expressions immediately without
;;;; creating Mach-O executables.

(in-package #:habu)

;;; ============================================================
;;; JIT Memory Constants (macOS ARM64)
;;; ============================================================

(defconstant +jit-page-size+ 16384)   ; 16KB pages on ARM64 macOS
(defconstant +jit-default-size+ 65536) ; 64KB default JIT region

;;; ============================================================
;;; JIT Runtime State
;;; ============================================================

;; Global JIT state (initialized by jit-init)
(defvar *jit-memory* nil)        ; mmap'd JIT memory region
(defvar *jit-memory-size* 0)     ; Size of JIT region
(defvar *jit-write-ptr* 0)       ; Current write position
(defvar *jit-fn-table* nil)      ; Function table for JIT calls
(defvar *jit-extern-table* nil)  ; Extern call trampolines
(defvar *jit-initialized* nil)   ; True if JIT runtime is initialized

;;; ============================================================
;;; JIT Initialization
;;; ============================================================

(defun jit-init (&optional (size +jit-default-size+))
  "Initialize JIT runtime.
   Allocates JIT memory and sets up extern trampolines.
   Must be called before any JIT compilation/execution."
  (unless *jit-initialized*
    ;; Allocate JIT memory
    (setf *jit-memory* (jit-mmap size))
    (when (or (null *jit-memory*) (= *jit-memory* -1))
      (error "Failed to allocate JIT memory"))
    (setf *jit-memory-size* size)
    (setf *jit-write-ptr* 0)
    ;; Initialize function table (empty initially)
    (setf *jit-fn-table* (make-hash-table :test 'eq))
    ;; Initialize extern trampolines
    (setf *jit-extern-table* (jit-setup-extern-trampolines))
    (setf *jit-initialized* t))
  t)

(defun jit-shutdown ()
  "Shutdown JIT runtime and free resources."
  (when *jit-initialized*
    ;; Note: jit-free is currently a no-op (munmap not implemented)
    ;; (jit-free *jit-memory* *jit-memory-size*)
    (setf *jit-memory* nil)
    (setf *jit-memory-size* 0)
    (setf *jit-write-ptr* 0)
    (setf *jit-fn-table* nil)
    (setf *jit-extern-table* nil)
    (setf *jit-initialized* nil))
  t)

;;; ============================================================
;;; Extern Trampolines
;;; ============================================================

;; The extern table maps extern names to trampoline addresses.
;; Each trampoline is a small piece of code that calls the
;; actual system function.

(defun jit-setup-extern-trampolines ()
  "Set up extern call trampolines.
   Returns hash table of name -> trampoline address."
  ;; For now, return empty table. Extern calls will use
  ;; the deliver path (markers resolved by linker).
  (make-hash-table :test 'equal))

;;; ============================================================
;;; Code Loading
;;; ============================================================

(defun jit-load (code-bytes)
  "Load code bytes into JIT memory.
   Returns pointer to executable code."
  (unless *jit-initialized*
    (jit-init))
  (let* ((size (length code-bytes))
         (aligned-size (* (1+ (floor size +jit-page-size+)) +jit-page-size+))
         (ptr (+ *jit-memory* *jit-write-ptr*)))
    ;; Check if we have space
    (when (> (+ *jit-write-ptr* aligned-size) *jit-memory-size*)
      (error "JIT memory exhausted"))
    ;; Enable writing
    (jit-write-protect 0)
    ;; Copy bytes
    (jit-copy-code ptr code-bytes size)
    ;; Flush caches
    (jit-dcache-flush ptr size)
    ;; Disable writing (enable execution)
    (jit-write-protect 1)
    ;; Invalidate instruction cache
    (jit-icache-invalidate ptr size)
    ;; Update write pointer
    (incf *jit-write-ptr* aligned-size)
    ;; Return code pointer
    ptr))

(defun jit-copy-code (dst code-bytes len)
  "Copy code bytes to destination address."
  (dotimes (i len)
    (mem-set-byte dst i (if (listp code-bytes)
                            (nth i code-bytes)
                            (aref code-bytes i)))))

;;; ============================================================
;;; Code Execution
;;; ============================================================

(defun jit-execute (code-ptr)
  "Execute JIT-compiled code at CODE-PTR.
   Returns the result (tagged fixnum in x0)."
  (jit-call code-ptr))

;;; ============================================================
;;; REPL Integration
;;; ============================================================

(defun jit-eval-expr (ir)
  "JIT compile and execute an IR expression.
   This is the main entry point for REPL evaluation."
  (let* ((ctx (make-jit-mode-context))
         (code (jit-generate ir ctx 0))
         (bytes (jit-flatten-code code))
         (ptr (jit-load bytes)))
    (jit-execute ptr)))

(defun jit-flatten-code (code)
  "Flatten code list to byte vector.
   Resolves any remaining markers."
  ;; For now, simple flattening (no marker resolution)
  ;; This works for simple expressions without function calls
  (let ((result nil))
    (labels ((flatten (items)
               (dolist (item items)
                 (cond
                   ((null item) nil)
                   ((integerp item) (push item result))
                   ((listp item)
                    (if (and (car item) (keywordp (car item)))
                        ;; Marker - skip for now
                        nil
                        (flatten item)))))))
      (flatten code))
    (nreverse result)))

;;; ============================================================
;;; Function Registration
;;; ============================================================

(defun jit-register-fn (name code-ptr)
  "Register a JIT-compiled function in the function table."
  (setf (gethash name *jit-fn-table*) code-ptr))

(defun jit-lookup-fn (name)
  "Look up a function in the JIT function table."
  (gethash name *jit-fn-table*))

;;; ============================================================
;;; Wrapper Code Generation
;;; ============================================================

(defun jit-wrap-expr (code)
  "Wrap expression code with minimal prologue/epilogue.
   Assumes heap (x28) and GC globals (x27) are already set up."
  (append
   ;; Prologue: save callee-saved registers we use
   (list (arm64:str :lr :sp :offset -16)    ; save return address
         (arm64:sub :sp :sp 16 :imm t))
   ;; User code
   code
   ;; Epilogue: restore and return
   (list (arm64:add :sp :sp 16 :imm t)
         (arm64:ldr :lr :sp :offset -16)
         (arm64:ret))))

;;; ============================================================
;;; Debug Utilities
;;; ============================================================

(defun jit-disassemble (code-bytes)
  "Disassemble JIT code bytes for debugging."
  (let ((hex-str (format nil "~{~2,'0X ~}" code-bytes)))
    (format t "JIT code (~D bytes):~%~A~%" (length code-bytes) hex-str)))
