#!/usr/bin/env sbcl --script
;;; Test SBCL compiler basic functionality

(load "sbcl-habu-shim.lisp")
(load "habu-arm64-codegen-sbcl.lisp")

(format t "~%=== Testing Habu ARM64 Compiler in SBCL ===~%~%")

;; Test 1: Compile a literal
(format t "Test 1: Compile literal 42~%")
(let* ((code-list (habu-sbcl-codegen:compile-to-arm64 42))
       (code (if (listp code-list)
                 (coerce code-list '(vector (unsigned-byte 8)))
                 code-list)))
  (format t "Generated ~D bytes of ARM64 code~%" (length code))
  (format t "First 16 bytes (hex): ")
  (loop for i from 0 below (min 16 (length code))
        do (format t "~2,'0X " (aref code i)))
  (format t "~%~%"))

;; Helper function to convert list to byte vector
(defun to-byte-vector (code-list)
  (if (listp code-list)
      (coerce code-list '(vector (unsigned-byte 8)))
      code-list))

;; Test 2: Compile addition
(format t "Test 2: Compile (+ 5 7)~%")
(let ((code (to-byte-vector (habu-sbcl-codegen:compile-to-arm64 '(+ 5 7)))))
  (format t "Generated ~D bytes of ARM64 code~%" (length code))
  (format t "First 16 bytes (hex): ")
  (loop for i from 0 below (min 16 (length code))
        do (format t "~2,'0X " (aref code i)))
  (format t "~%~%"))

;; Test 3: Compile multiplication
(format t "Test 3: Compile (* 6 7)~%")
(let ((code (to-byte-vector (habu-sbcl-codegen:compile-to-arm64 '(* 6 7)))))
  (format t "Generated ~D bytes of ARM64 code~%" (length code))
  (format t "First 16 bytes (hex): ")
  (loop for i from 0 below (min 16 (length code))
        do (format t "~2,'0X " (aref code i)))
  (format t "~%~%"))

;; Test 4: Compile conditional
(format t "Test 4: Compile (if (= 5 5) 42 99)~%")
(let ((code (to-byte-vector (habu-sbcl-codegen:compile-to-arm64 '(if (= 5 5) 42 99)))))
  (format t "Generated ~D bytes of ARM64 code~%" (length code))
  (format t "First 16 bytes (hex): ")
  (loop for i from 0 below (min 16 (length code))
        do (format t "~2,'0X " (aref code i)))
  (format t "~%~%"))

(format t "=== All tests completed ===~%")
