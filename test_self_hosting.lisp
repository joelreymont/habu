#!/usr/bin/env sbcl --script
;;; Test self-hosting compilation pipeline

(load "bootstrap/compiler.lisp")

(defun run-and-get-exit-code (path)
  (sb-ext:process-exit-code (sb-ext:run-program path nil :wait t :search nil)))

(format t "~%=== Habu Self-Hosting Compilation Test ===~%~%")

;; Test 1: Simple arithmetic
(format t "Test 1: Simple arithmetic... ")
(habu:deliver-with-libsystem "(sys-exit (+ 20 22))" "/tmp/test_arith")
(let ((result (run-and-get-exit-code "/tmp/test_arith")))
  (if (= result 42)
      (format t "PASS~%")
      (format t "FAIL (expected 42, got ~A)~%" result)))

;; Test 2: Factorial
(format t "Test 2: Factorial... ")
(habu:deliver-with-libsystem 
  "(defun fact (n acc) (if (= n 0) acc (fact (- n 1) (* n acc)))) (sys-exit (fact 5 1))"
  "/tmp/test_fact2")
(let ((result (run-and-get-exit-code "/tmp/test_fact2")))
  (if (= result 120)
      (format t "PASS~%")
      (format t "FAIL (expected 120, got ~A)~%" result)))

;; Test 3: Closures
(format t "Test 3: Closures... ")
(habu:deliver-with-libsystem
  "(defun make-adder (n) (lambda (x) (+ x n))) (let ((add10 (make-adder 10))) (sys-exit (funcall add10 32)))"
  "/tmp/test_closure2")
(let ((result (run-and-get-exit-code "/tmp/test_closure2")))
  (if (= result 42)
      (format t "PASS~%")
      (format t "FAIL (expected 42, got ~A)~%" result)))

;; Test 4: Labels (mutual recursion)
(format t "Test 4: Labels (mutual recursion)... ")
(habu:deliver-with-libsystem
  "(labels ((even? (n) (if (= n 0) 1 (odd? (- n 1)))) (odd? (n) (if (= n 0) 0 (even? (- n 1))))) (sys-exit (even? 10)))"
  "/tmp/test_mutual")
(let ((result (run-and-get-exit-code "/tmp/test_mutual")))
  (if (= result 1)
      (format t "PASS~%")
      (format t "FAIL (expected 1, got ~A)~%" result)))

;; Test 5: Self-compilation
(format t "Test 5: Self-compilation (large program)... ")
(habu:deliver-file-with-libsystem "bootstrap/compiler.lisp" "/tmp/habu-self-test")
(let ((size (with-open-file (in "/tmp/habu-self-test" :element-type '(unsigned-byte 8))
              (file-length in))))
  (if (> size 1000000)  ; Should be > 1MB
      (format t "PASS (generated ~:D bytes)~%" size)
      (format t "FAIL (expected > 1MB, got ~:D bytes)~%" size)))

(format t "~%=== All Tests Complete ===~%")
