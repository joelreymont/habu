;;;; Test simple compilation with fixed compiler

(load "run-habu.lisp")

(in-package :habu-sbcl-codegen)

(defun dump-bytes (bytes)
  "Print bytes as hex"
  (loop for b in bytes
        for i from 0
        do (progn
             (when (and (> i 0) (zerop (mod i 16)))
               (format t "~%"))
             (format t "~2,'0X " b)))
  (format t "~%"))

(format t "~%=== Testing Fixed Compiler ===~%~%")

;; Test 1: Simple literal
(format t "Test 1: Compiling literal 42~%")
(let ((bytes (compile-to-arm64-with-runtime 42 *runtime-addrs*)))
  (format t "Generated ~D bytes~%" (length bytes))
  (dump-bytes bytes))

;; Test 2: Simple addition
(format t "~%Test 2: Compiling (+ 10 32)~%")
(let ((bytes (compile-to-arm64-with-runtime '(+ 10 32) *runtime-addrs*)))
  (format t "Generated ~D bytes~%" (length bytes))
  (dump-bytes bytes))

(format t "~%Done~%")
