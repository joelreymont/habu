# Trace Implementation Plan

## Overview

Implement CL-spec compliant `trace`/`untrace` for Habu with three approaches:
1. Interpreter hook in `eval-ir-with-fns`
2. Compiled code instrumentation in `codegen`
3. Function wrapper via symbol-function

## Phase 1: Core Infrastructure (trace.lisp)

Create `bootstrap/trace.lisp` with:

```lisp
(in-package :habu)

;;; Trace state
(defvar *traced-functions* (make-hash-table :test 'eq))
(defvar *trace-depth* 0)
(defvar *trace-output* *trace-output*)

;;; Output helpers
(defun trace-indent (stream)
  (dotimes (i (* 2 *trace-depth*))
    (write-char #\Space stream)))

(defun trace-enter (name args)
  (trace-indent *trace-output*)
  (format *trace-output* "~D: (~S~{ ~S~})~%" *trace-depth* name args))

(defun trace-exit (name value)
  (trace-indent *trace-output*)
  (format *trace-output* "~D: ~S returned ~S~%" *trace-depth* name value))

;;; Public API
(defun trace-function (name)
  (setf (gethash name *traced-functions*) t)
  name)

(defun untrace-function (name)
  (remhash name *traced-functions*)
  name)

(defun traced-p (name)
  (gethash name *traced-functions*))

(defun list-traced ()
  (let ((result nil))
    (maphash (lambda (k v) (declare (ignore v)) (push k result))
             *traced-functions*)
    result))

(defmacro trace (&rest names)
  (if names
      `(progn ,@(mapcar (lambda (n) `(trace-function ',n)) names)
              (list-traced))
      '(list-traced)))

(defmacro untrace (&rest names)
  (if names
      `(progn ,@(mapcar (lambda (n) `(untrace-function ',n)) names)
              (list-traced))
      `(progn
         (dolist (n (list-traced)) (untrace-function n))
         nil)))
```

## Phase 2: Interpreter Hook (eval-ir-with-fns)

Modify `call-fn` handling in `eval-ir-with-fns` (~line 2627):

```lisp
((has-tag ir 'call-fn)
 (let* ((fnm (cadr ir))
        (args-ir (caddr ir))
        (fn-def (cdr (assoc fnm fenv)))
        (traced (traced-p fnm)))  ; NEW
   (if fn-def
       (let* ((body-ir (caddr fn-def)))
         (labels ((eval-args (airs acc)
                    (if (null airs) (reverse acc)
                        (eval-args (cdr airs)
                                   (cons (eval-ir-with-fns (car airs) env fenv) acc)))))
           (let ((arg-vals (eval-args args-ir nil)))
             ;; NEW: trace entry
             (when traced
               (trace-enter fnm arg-vals)
               (incf *trace-depth*))
             (let ((result (eval-ir-with-fns body-ir arg-vals fenv)))
               ;; NEW: trace exit
               (when traced
                 (decf *trace-depth*)
                 (trace-exit fnm result))
               result))))
       0)))
```

Similarly modify `funcall-ir` handling (~line 2644).

## Phase 3: Compiled Code Instrumentation (codegen)

Add trace calls in codegen for `call-fn` IR (~line 4066):

Option A: Always emit trace check (overhead when not tracing)
```lisp
;; At function entry, before evaluating args:
(when *compile-with-trace-support*
  ;; Load trace flag for this function
  ;; If set, call trace-enter runtime
  )
```

Option B: Conditional compilation flag
```lisp
(defvar *compile-with-trace-support* nil)

;; In codegen for call-fn:
(when *compile-with-trace-support*
  (emit-trace-check fnm))
```

For native trace support, need runtime functions:
- `rt-trace-enter`: Print function entry
- `rt-trace-exit`: Print function exit
- `rt-trace-check`: Check if function is traced

These would be added to the runtime (like `rt-print-int`).

## Phase 4: Function Wrapper (symbol-function style)

Store original function definitions and wrap:

```lisp
(defvar *original-fn-defs* (make-hash-table :test 'eq))

(defun wrap-for-trace (name)
  "Create tracing wrapper for function NAME in fenv."
  (let ((fn-def (assoc name *global-fenv*)))
    (when fn-def
      ;; Store original
      (setf (gethash name *original-fn-defs*) (cdr fn-def))
      ;; Replace with wrapper that traces
      ;; (This requires creating a new fn-def that calls trace-enter/exit)
      )))

(defun unwrap-trace (name)
  "Restore original function definition."
  (let ((original (gethash name *original-fn-defs*)))
    (when original
      (setf (cdr (assoc name *global-fenv*)) original)
      (remhash name *original-fn-defs*))))
```

## File Changes

1. **NEW: bootstrap/trace.lisp** - Trace infrastructure
2. **MODIFY: bootstrap/compiler-sbcl.lisp** - Add trace hooks to eval-ir-with-fns
3. **MODIFY: bootstrap/habu.asd** - Add trace.lisp to system
4. **MODIFY: bootstrap/compiler-sbcl.lisp** - Export trace, untrace

## Implementation Order

1. Create trace.lisp with core infrastructure
2. Add to habu.asd
3. Export symbols from package
4. Modify eval-ir-with-fns for call-fn
5. Modify eval-ir-with-fns for funcall-ir
6. Test with simple traced function
7. (Optional) Add compiled code support later

## Test Cases

```lisp
;; Basic trace
(defun fact (n)
  (if (<= n 1) 1 (* n (fact (- n 1)))))

(trace fact)
(fact 5)
;; Should print:
;; 0: (FACT 5)
;;   1: (FACT 4)
;;     2: (FACT 3)
;;       3: (FACT 2)
;;         4: (FACT 1)
;;         4: FACT returned 1
;;       3: FACT returned 2
;;     2: FACT returned 6
;;   1: FACT returned 24
;; 0: FACT returned 120

(untrace fact)
(trace)  ; => NIL
```
