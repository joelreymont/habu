;;;; Test loading the REAL compiler (without #-sbcl wrapper) in SBCL
;;;;
;;;; Strategy: Read habu-arm64-codegen.lisp, skip the #-sbcl wrapper,
;;;; and load the actual compiler code.

(load "sbcl-habu-shim.lisp")

(defpackage :habu-real-codegen
  (:use :cl :habu-shim)
  (:export :compile-to-arm64 :compile-to-arm64-with-runtime
           :compile-program-with-functions-with-runtime))

(in-package :habu-real-codegen)

(format t "~%=== Loading Real Compiler in SBCL ===~%~%")

;;; Read the file and skip the #-sbcl wrapper
(format t "Reading habu-arm64-codegen.lisp...~%")

(let ((forms-to-eval '()))
  ;; Read all forms from the file
  (with-open-file (stream "habu-arm64-codegen.lisp")
    ;; Skip the #-sbcl line
    (read-line stream)
    ;; Skip the (progn line
    (read stream)

    ;; Read forms until we hit the closing paren
    (loop
      (let ((form (read stream nil 'eof)))
        (when (eq form 'eof)
          (return))
        (push form forms-to-eval))))

  ;; Reverse to get original order
  (setf forms-to-eval (nreverse forms-to-eval))

  ;; Remove the last form if it's just a closing paren comment
  (when (and (consp (car (last forms-to-eval)))
             (eq (caar (last forms-to-eval)) 'quote))
    (setf forms-to-eval (butlast forms-to-eval)))

  (format t "Found ~D forms to evaluate~%~%" (length forms-to-eval))

  ;; Evaluate each form
  (let ((count 0)
        (errors 0))
    (dolist (form forms-to-eval)
      (incf count)
      (handler-case
          (progn
            (eval form)
            (when (and (consp form) (eq (car form) 'defun))
              (format t "  ✓ Loaded: ~A~%" (cadr form))))
        (error (e)
          (incf errors)
          (format t "  ✗ Error loading form ~D: ~A~%" count e))))

    (format t "~%Loaded ~D forms (~D errors)~%~%" (- count errors) errors)))

(format t "=== Testing Real Compiler ===~%~%")

;;; Test 1: Compile a literal
(format t "Test 1: Compile literal 42~%")
(handler-case
    (let ((code (compile-to-arm64 42)))
      (format t "✓ Success! Generated ~D bytes~%~%" (length code)))
  (error (e)
    (format t "✗ Failed: ~A~%~%" e)))

;;; Test 2: Compile arithmetic
(format t "Test 2: Compile (+ 2 3)~%")
(handler-case
    (let ((code (compile-to-arm64 '(+ 2 3))))
      (format t "✓ Success! Generated ~D bytes~%~%" (length code)))
  (error (e)
    (format t "✗ Failed: ~A~%~%" e)))

(format t "=== Summary ===~%")
(format t "If this worked, we can now use the REAL compiler in SBCL!~%")
(format t "Next: Generate functional bytecode and compare with hand-written patterns~%~%")
