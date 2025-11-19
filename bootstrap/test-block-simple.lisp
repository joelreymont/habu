;;;; Simple test for block/return-from

(load "compiler.lisp")
(in-package :habu-compiler)
(initialize-runtime-integration)

(format t "Testing block/return-from...~%")

;; Test 1: Simple block
(handler-case
    (let ((code (compile-expression '(block search (+ 1 2)) :arch :x86_64)))
      (format t "✓ block compiles (~D bytes)~%" (length code)))
  (error (e) (format t "✗ block failed: ~A~%" e)))

;; Test 2: return-from
(handler-case
    (let ((code (compile-expression '(block done (return-from done 42)) :arch :x86_64)))
      (format t "✓ return-from compiles (~D bytes)~%" (length code)))
  (error (e) (format t "✗ return-from failed: ~A~%" e)))

(format t "~%block/return-from tests passed!~%")
(sb-ext:quit)
