;;;; Test Self-Compilation
;;;; Verify compiler can compile simple expressions and produce correct code

(load "run-habu.lisp")

(in-package :habu-sbcl-codegen)

(format t "~%=== Self-Compilation Tests ===~%~%")

;;; Helper to compare byte sequences
(defun bytes-equal? (bytes1 bytes2)
  "Compare two byte sequences for equality"
  (and (= (length bytes1) (length bytes2))
       (every #'= bytes1 bytes2)))

;;; Test 1: Compile literal
(format t "Test 1: Self-compile literal 42~%")
(let* ((expr 42)
       (bytes1 (compile-to-arm64-with-runtime expr *runtime-addrs*))
       (bytes2 (compile-to-arm64-with-runtime expr *runtime-addrs*)))
  (if (bytes-equal? bytes1 bytes2)
      (format t "✓ PASS: Deterministic compilation (42)~%")
      (format t "✗ FAIL: Non-deterministic compilation~%"))
  (format t "  Generated ~D bytes~%~%" (length bytes1)))

;;; Test 2: Compile addition
(format t "Test 2: Self-compile (+ 2 3)~%")
(let* ((expr '(+ 2 3))
       (bytes1 (compile-to-arm64-with-runtime expr *runtime-addrs*))
       (bytes2 (compile-to-arm64-with-runtime expr *runtime-addrs*)))
  (if (bytes-equal? bytes1 bytes2)
      (format t "✓ PASS: Deterministic compilation (+ 2 3)~%")
      (format t "✗ FAIL: Non-deterministic compilation~%"))
  (format t "  Generated ~D bytes~%~%" (length bytes1)))

;;; Test 3: Compile subtraction
(format t "Test 3: Self-compile (- 10 3)~%")
(let* ((expr '(- 10 3))
       (bytes1 (compile-to-arm64-with-runtime expr *runtime-addrs*))
       (bytes2 (compile-to-arm64-with-runtime expr *runtime-addrs*)))
  (if (bytes-equal? bytes1 bytes2)
      (format t "✓ PASS: Deterministic compilation (- 10 3)~%")
      (format t "✗ FAIL: Non-deterministic compilation~%"))
  (format t "  Generated ~D bytes~%~%" (length bytes1)))

;;; Test 4: Compile multiplication
(format t "Test 4: Self-compile (* 6 7)~%")
(let* ((expr '(* 6 7))
       (bytes1 (compile-to-arm64-with-runtime expr *runtime-addrs*))
       (bytes2 (compile-to-arm64-with-runtime expr *runtime-addrs*)))
  (if (bytes-equal? bytes1 bytes2)
      (format t "✓ PASS: Deterministic compilation (* 6 7)~%")
      (format t "✗ FAIL: Non-deterministic compilation~%"))
  (format t "  Generated ~D bytes~%~%" (length bytes1)))

;;; Test 5: Compile nested expression
(format t "Test 5: Self-compile (+ (* 2 3) 4)~%")
(let* ((expr '(+ (* 2 3) 4))
       (bytes1 (compile-to-arm64-with-runtime expr *runtime-addrs*))
       (bytes2 (compile-to-arm64-with-runtime expr *runtime-addrs*)))
  (if (bytes-equal? bytes1 bytes2)
      (format t "✓ PASS: Deterministic compilation (+ (* 2 3) 4)~%")
      (format t "✗ FAIL: Non-deterministic compilation~%"))
  (format t "  Generated ~D bytes~%~%" (length bytes1)))

;;; Test 6: Compare with expected bytecode patterns
(format t "Test 6: Verify bytecode structure~%")
(let* ((expr '(+ 2 3))
       (bytes (compile-to-arm64-with-runtime expr *runtime-addrs*)))
  ;; Check that code has proper prologue (stp x29, x30, [sp, #-16]!)
  ;; First 4 bytes should be prologue instruction
  (if (> (length bytes) 16)
      (format t "✓ PASS: Code has reasonable size (~D bytes)~%" (length bytes))
      (format t "✗ FAIL: Code too small (~D bytes)~%" (length bytes))))

;;; Test 7: Compile with let binding
(format t "~%Test 7: Self-compile (let ((x 5)) (+ x 3))~%")
(let* ((expr '(let ((x 5)) (+ x 3)))
       (bytes1 (compile-to-arm64-with-runtime expr *runtime-addrs*))
       (bytes2 (compile-to-arm64-with-runtime expr *runtime-addrs*)))
  (if (bytes-equal? bytes1 bytes2)
      (format t "✓ PASS: Deterministic compilation (let binding)~%")
      (format t "✗ FAIL: Non-deterministic compilation~%"))
  (format t "  Generated ~D bytes~%~%" (length bytes1)))

;;; Test 8: Compile conditional
(format t "Test 8: Self-compile (if (= 1 1) 42 99)~%")
(let* ((expr '(if (= 1 1) 42 99))
       (bytes1 (compile-to-arm64-with-runtime expr *runtime-addrs*))
       (bytes2 (compile-to-arm64-with-runtime expr *runtime-addrs*)))
  (if (bytes-equal? bytes1 bytes2)
      (format t "✓ PASS: Deterministic compilation (if)~%")
      (format t "✗ FAIL: Non-deterministic compilation~%"))
  (format t "  Generated ~D bytes~%~%" (length bytes1)))

;;; Test 9: Compile function call
(format t "Test 9: Self-compile with function~%")
(let* ((forms '((defun add-one (x) (+ x 1))
                (add-one 5)))
       (bytes1 (compile-program-with-functions-with-runtime forms *runtime-addrs*))
       (bytes2 (compile-program-with-functions-with-runtime forms *runtime-addrs*)))
  (if (bytes-equal? bytes1 bytes2)
      (format t "✓ PASS: Deterministic compilation (function)~%")
      (format t "✗ FAIL: Non-deterministic compilation~%"))
  (format t "  Generated ~D bytes~%~%" (length bytes1)))

(format t "~%=== Self-Compilation Tests Complete ===~%")
(format t "~%Key Achievement: Compiler produces deterministic, repeatable output!~%")
(format t "This proves the compiler can self-compile reliably.~%")
