; Test simple program compilation using stub codegen

(load "run-habu.lisp")

(in-package :habu-sbcl-codegen)

(format t "~%=== Testing Simple Program Compilation ===~%")

; Test 1: Compile a literal
(format t "~%Test 1: Compile literal 42~%")
(let ((bytes (compile-to-arm64-with-runtime 42 *runtime-addrs*)))
  (format t "Generated ~A bytes~%" (length bytes)))

; Test 2: Compile simple expression
(format t "~%Test 2: Compile (+ 3 4)~%")
(let ((bytes (compile-to-arm64-with-runtime '(+ 3 4) *runtime-addrs*)))
  (format t "Generated ~A bytes~%" (length bytes)))

(format t "~%All compilation tests completed!~%")
