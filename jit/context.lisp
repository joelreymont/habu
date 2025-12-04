;;;; JIT Context - Compilation context for JIT and deliver modes
;;;;
;;;; This module provides the context object that controls how code
;;;; is generated. Both REPL (JIT) and deliver use the same core
;;;; codegen but with different contexts.

(in-package #:habu)

;;; ============================================================
;;; JIT Context Structure
;;; ============================================================

;; Context holds all state needed during code generation
;; Mode determines how function calls and extern calls are resolved:
;;   :jit - calls through runtime tables (for REPL)
;;   :deliver - generates markers for later resolution (for Mach-O)

(defstruct jit-context
  (mode :deliver)           ; :jit or :deliver
  (fn-table nil)            ; Runtime function table (for :jit mode)
  (extern-table nil)        ; Runtime extern call table (for :jit mode)
  (fn-offsets nil)          ; Function offset alist (for :deliver mode)
  (current-offset 0)        ; Current code offset (for branch computation)
  (loop-stack nil)          ; Stack of loop start offsets
  (labels nil))             ; Label -> offset mapping

;;; ============================================================
;;; Context Constructors
;;; ============================================================

(defun make-jit-mode-context (&key fn-table extern-table)
  "Create context for JIT/REPL mode.
   FN-TABLE: runtime address of function table
   EXTERN-TABLE: runtime address of extern call trampolines"
  (make-jit-context
   :mode :jit
   :fn-table fn-table
   :extern-table extern-table))

(defun make-deliver-mode-context (&key fn-offsets)
  "Create context for deliver mode (Mach-O generation).
   FN-OFFSETS: alist of (fn-name . offset) for function calls"
  (make-jit-context
   :mode :deliver
   :fn-offsets fn-offsets))

;;; ============================================================
;;; Context Accessors
;;; ============================================================

(defun jit-mode-p (ctx)
  "True if context is for JIT/REPL mode."
  (eq (jit-context-mode ctx) :jit))

(defun deliver-mode-p (ctx)
  "True if context is for deliver mode."
  (eq (jit-context-mode ctx) :deliver))

;;; ============================================================
;;; Offset Tracking
;;; ============================================================

(defun ctx-advance (ctx bytes)
  "Advance current offset by BYTES."
  (incf (jit-context-current-offset ctx) bytes))

(defun ctx-current-offset (ctx)
  "Get current code offset."
  (jit-context-current-offset ctx))

(defun ctx-reset-offset (ctx)
  "Reset offset to 0 (for new compilation unit)."
  (setf (jit-context-current-offset ctx) 0))

;;; ============================================================
;;; Loop Stack Management
;;; ============================================================

(defun ctx-push-loop (ctx offset)
  "Push loop start offset onto stack."
  (push offset (jit-context-loop-stack ctx)))

(defun ctx-pop-loop (ctx)
  "Pop and return loop start offset."
  (pop (jit-context-loop-stack ctx)))

(defun ctx-current-loop (ctx)
  "Get current loop start offset (top of stack)."
  (car (jit-context-loop-stack ctx)))

;;; ============================================================
;;; Label Management
;;; ============================================================

(defun ctx-define-label (ctx name offset)
  "Define a label at the given offset."
  (push (cons name offset) (jit-context-labels ctx)))

(defun ctx-lookup-label (ctx name)
  "Look up a label's offset. Returns nil if not found."
  (cdr (assoc name (jit-context-labels ctx))))

;;; ============================================================
;;; Function Resolution
;;; ============================================================

(defun ctx-resolve-fn (ctx fn-name)
  "Resolve function call for the given context mode.
   In :jit mode, returns code to load fn address from table.
   In :deliver mode, returns a :call-fn marker."
  (if (jit-mode-p ctx)
      ;; JIT mode: generate indirect call through fn-table
      ;; (This will be implemented when we have runtime tables)
      (error "JIT function calls not yet implemented")
      ;; Deliver mode: return marker for later resolution
      (list :call-fn fn-name)))

(defun ctx-resolve-extern (ctx extern-name)
  "Resolve extern call for the given context mode.
   In :jit mode, returns code to call through extern-table.
   In :deliver mode, returns an :extern-call marker."
  (if (jit-mode-p ctx)
      ;; JIT mode: generate indirect call through extern-table
      (error "JIT extern calls not yet implemented")
      ;; Deliver mode: return marker for later resolution
      (list :extern-call extern-name)))
