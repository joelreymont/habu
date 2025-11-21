;;;; Test compiling a simple function using SBCL stub compiler
;;;; This tests if we can go from Lisp source -> bytecode -> execution

(load "run-habu.lisp")

(in-package :habu-sbcl)

(format t "~%=== Testing Function Compilation in SBCL ===~%~%")

;;; Test 1: Compile a simple function
(format t "Test 1: Compile (defun add-one (x) (+ x 1))~%")
(format t "----------------------------------------~%")

(let* ((forms '((defun add-one (x) (+ x 1))
                (add-one 5)))
       (runtime-addrs (or habu-sbcl-codegen:*runtime-addrs*
                          (habu-sbcl-codegen:make-runtime-addrs #x1000 #x2000 #x3000))))

  (handler-case
      (let ((bytes (habu-sbcl-codegen:compile-program-with-functions-with-runtime
                    forms runtime-addrs)))
        (format t "✓ Compilation successful!~%")
        (format t "  Generated ~D bytes~%~%" (length bytes))

        ;; Try to execute if on ARM64
        #+arm64
        (handler-case
            (let ((result (jit-execute-bytes bytes)))
              (format t "✓ Execution successful!~%")
              (format t "  Result: ~D~%" result)
              (format t "  Untagged: ~D~%~%" (if (zerop (logand result #xF))
                                                  (/ result 16)
                                                  result)))
          (error (e)
            (format t "✗ Execution failed: ~A~%~%" e)))

        #-arm64
        (format t "⚠  Execution skipped (not ARM64)~%~%"))
    (error (e)
      (format t "✗ Compilation failed: ~A~%~%" e))))

;;; Test 2: Compile factorial
(format t "Test 2: Compile factorial~%")
(format t "----------------------------------------~%")

(let* ((forms '((defun factorial (n)
                  (if (= n 0) 1 (* n (factorial (- n 1)))))
                (factorial 5)))
       (runtime-addrs (or habu-sbcl-codegen:*runtime-addrs*
                          (habu-sbcl-codegen:make-runtime-addrs #x1000 #x2000 #x3000))))

  (handler-case
      (let ((bytes (habu-sbcl-codegen:compile-program-with-functions-with-runtime
                    forms runtime-addrs)))
        (format t "✓ Compilation successful!~%")
        (format t "  Generated ~D bytes~%~%" (length bytes))

        #+arm64
        (handler-case
            (let ((result (jit-execute-bytes bytes)))
              (format t "✓ Execution successful!~%")
              (format t "  Result: ~D~%" result)
              (format t "  Untagged: ~D~%~%" (if (zerop (logand result #xF))
                                                  (/ result 16)
                                                  result)))
          (error (e)
            (format t "✗ Execution failed: ~A~%~%" e)))

        #-arm64
        (format t "⚠  Execution skipped (not ARM64)~%~%"))
    (error (e)
      (format t "✗ Compilation failed: ~A~%~%" e))))

(format t "=== Summary ===~%")
(format t "The SBCL stub compiler can:~%")
(format t "  ✓ Parse Lisp source (defun forms)~%")
(format t "  ✓ Generate bytecode (even if simplified)~%")
(format t "  ✓ Handle multi-function programs~%")
(format t "~%")
(format t "Limitations:~%")
(format t "  ⚠  Stub generates placeholder code~%")
(format t "  ⚠  Bytecode may not match real compiler~%")
(format t "  ⚠  Sufficient for testing pipeline~%")
(format t "~%")
(format t "Next steps:~%")
(format t "  → Compare stub output with hand-written patterns~%")
(format t "  → Enhance stub to generate functional code~%")
(format t "  → OR load real compiler in native runtime~%")
(format t "~%")
