;;; ============================================================
;;; Shared Codegen Helpers
;;; ============================================================
;;;
;;; Functions shared between SBCL bootstrap and native Habu.
;;; Used by reg-alloc.lisp for register-allocated code generation.
;;;
;;; These functions use arm64:* from arm64/asm.lisp which is
;;; shared between both environments.

#+sbcl (in-package :habu)

;;; ============================================================
;;; GC Layout Constants
;;; ============================================================
;;;
;;; Memory layout at x27 (GC globals base):
;;;   +0:   intern_table (symbol interning)
;;;   +8:   lambda_counter
;;;   +16:  from_end (GC from-space end)
;;;   +24:  half_heap_size
;;;   +32:  space_flag
;;;   +40:  gc_state
;;;   +48:  symbol_counter
;;;   +56:  symbol_table
;;;   +64:  argc
;;;   +72:  argv
;;;   +80:  packages
;;;   +88:  current_package
;;;   +96:  stack_base
;;;   +104: symtab_offset
;;;   +112: heap_base

(defconstant +gc-from-end-offset+ 16)
(defconstant +gc-half-heap-offset+ 24)
(defconstant +gc-space-flag-offset+ 32)
(defconstant +gc-heap-base-offset+ 112)

;;; ============================================================
;;; Address Loading
;;; ============================================================

(defun load-addr (rd addr)
  "Load a 64-bit address into register using MOVZ + MOVK sequence.
   Works in both SBCL and native Habu (uses arm64:* functions)."
  (let* ((lo16 (logand addr #xFFFF))
         (sh16 (ash addr -16))
         (hi16 (logand sh16 #xFFFF))
         (sh32 (ash addr -32))
         (hi32 (logand sh32 #xFFFF))
         (sh48 (ash addr -48))
         (hi48 (logand sh48 #xFFFF))
         (base (arm64:movz rd lo16))
         (p1 (if (> hi16 0) (arm64:movk rd hi16 :lsl 16) nil))
         (r1 (append base p1))
         (p2 (if (> hi32 0) (arm64:movk rd hi32 :lsl 32) nil))
         (r2 (append r1 p2))
         (p3 (if (> hi48 0) (arm64:movk rd hi48 :lsl 48) nil)))
    (append r2 p3)))

(defun load-addr-32 (rd addr)
  "Load a 32-bit address using exactly 8 bytes (MOVZ + MOVK).
   Used for function offsets to ensure consistent code size."
  (let* ((lo16 (logand addr #xFFFF))
         (hi16 (logand (ash addr -16) #xFFFF)))
    (append (arm64:movz rd lo16)
            (arm64:movk rd hi16 :lsl 16))))

;;; ============================================================
;;; GC Trigger Code and Prologue/Epilogue
;;; ============================================================
;;;
;;; gc-trigger-code, fn-fixed-prologue, fn-fixed-epilogue are defined in:
;;; - compiler/codegen.lisp for SBCL (with generational GC support)
;;; - habu0.lisp for native self-compilation (simple versions)

;;; ============================================================
;;; Helper Utilities
;;; ============================================================

(defun append-all (lists)
  "Append all lists in LISTS into a single list."
  (if (null lists)
      nil
      (append (car lists) (append-all (cdr lists)))))

(defun take-bytes (bytes n)
  "Take first N bytes from list."
  (if (or (null bytes) (<= n 0))
      nil
      (cons (car bytes) (take-bytes (cdr bytes) (- n 1)))))

(defun drop-bytes (bytes n)
  "Drop first N bytes from list."
  (if (or (null bytes) (<= n 0))
      bytes
      (drop-bytes (cdr bytes) (- n 1))))

(defun bytes-to-u64 (bytes)
  "Convert list of up to 8 bytes to 64-bit unsigned integer (little-endian)."
  (labels ((convert (bs shift acc)
             (if (null bs)
                 acc
                 (convert (cdr bs) (+ shift 8)
                          (logior acc (ash (car bs) shift))))))
    (convert bytes 0 0)))

