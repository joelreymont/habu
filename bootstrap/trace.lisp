;;;; trace.lisp - CL-spec compliant trace facility for Habu
;;;;
;;;; Provides (trace name ...) and (untrace name ...) macros
;;;; per Common Lisp specification.
;;;;
;;;; Note: The trace state variables and helper functions are defined
;;;; in compiler-sbcl.lisp to avoid circular dependencies.

(in-package :habu)

;;; ==========================================================
;;; Public Macros (CL-spec interface)
;;; ==========================================================

(defmacro trace (&rest names)
  "Enable tracing for the named functions.
   (trace name ...)  - Enable tracing, returns list of traced functions
   (trace)           - Returns list of currently traced functions"
  (if names
      `(progn
         ,@(mapcar (lambda (n) `(trace-function ',n)) names)
         (list-traced))
      '(list-traced)))

(defmacro untrace (&rest names)
  "Disable tracing for the named functions.
   (untrace name ...) - Disable tracing for named functions
   (untrace)          - Disable tracing for all functions"
  (if names
      `(progn
         ,@(mapcar (lambda (n) `(untrace-function ',n)) names)
         (list-traced))
      `(progn
         (dolist (n (list-traced))
           (untrace-function n))
         nil)))
