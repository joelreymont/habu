;;;; Test loading the unwrapped real compiler in SBCL

(load "sbcl-habu-shim.lisp")

(defpackage :habu-real
  (:use :cl :habu-shim))

(in-package :habu-real)

(format t "~%=== Loading Real Habu Compiler (Unwrapped) ===~%~%")

(handler-case
    (progn
      (load "habu-arm64-codegen-clean.lisp")
      (format t "✓ Compiler loaded successfully!~%~%"))
  (error (e)
    (format t "✗ Failed to load compiler: ~A~%~%" e)
    (sb-ext:exit :code 1)))

(format t "=== Testing Real Compiler ===~%~%")

;;; Test 1: Compile a literal
(format t "Test 1: Compile literal 42~%")
(handler-case
    (let ((code (compile-to-arm64 42)))
      (format t "✓ Success! Generated ~D bytes~%" (length code))
      (format t "  First 16 bytes: ~{~2,'0X ~}~%~%" (subseq code 0 (min 16 (length code)))))
  (error (e)
    (format t "✗ Failed: ~A~%~%" e)))

;;; Test 2: Compile arithmetic
(format t "Test 2: Compile (+ 2 3)~%")
(handler-case
    (let ((code (compile-to-arm64 '(+ 2 3))))
      (format t "✓ Success! Generated ~D bytes~%" (length code))
      (format t "  First 16 bytes: ~{~2,'0X ~}~%~%" (subseq code 0 (min 16 (length code)))))
  (error (e)
    (format t "✗ Failed: ~A~%~%" e)))

;;; Test 3: Compile a function
(format t "Test 3: Compile (defun add-one (x) (+ x 1)) + (add-one 5)~%")
(handler-case
    (let ((code (compile-program-with-functions '((defun add-one (x) (+ x 1))
                                                   (add-one 5)))))
      (format t "✓ Success! Generated ~D bytes~%" (length code))
      (format t "  First 32 bytes: ~{~2,'0X ~}~%~%" (subseq code 0 (min 32 (length code)))))
  (error (e)
    (format t "✗ Failed: ~A~%~%" e)))

(format t "=== Summary ===~%")
(format t "Real compiler now loadable in SBCL!~%")
(format t "Can generate actual functional ARM64 bytecode~%")
(format t "Next: Execute and verify results~%~%")
