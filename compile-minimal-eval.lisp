;;;; Compile minimal evaluator and test
;;;; Tests if evaluator functions can be compiled to ARM64

(load "run-habu.lisp")

(in-package :habu-sbcl-codegen)

(defun dump-bytes-as-hex (bytes)
  "Dump bytes as hex for inspection"
  (format t "Generated ~D bytes:~%" (length bytes))
  (loop for b in bytes
        for i from 0
        do (progn
             (when (and (> i 0) (zerop (mod i 16)))
               (format t "~%"))
             (format t "~2,'0X " b)))
  (format t "~%~%"))

(format t "~%=== Compiling Minimal Evaluator ===~%~%")

;; Test 1: Compile simple arithmetic operations
(format t "Test 1: Compile arithmetic operations~%")
(let ((forms '(
  (defun add-two (a b)
    (+ a b))
  (add-two 10 5))))
  (handler-case
      (let ((bytes (compile-program-with-functions-with-runtime forms *runtime-addrs*)))
        (format t "✓ SUCCESS: add-two compiled (~D bytes)~%~%" (length bytes))
        bytes)
    (error (e)
      (format t "✗ FAIL: ~A~%~%" e)
      nil)))

;; Test 2: Compile with conditionals
(format t "Test 2: Compile with conditionals~%")
(let ((forms '(
  (defun test-if (x)
    (if (= x 0)
        10
        20))
  (test-if 0))))
  (handler-case
      (let ((bytes (compile-program-with-functions-with-runtime forms *runtime-addrs*)))
        (format t "✓ SUCCESS: test-if compiled (~D bytes)~%~%" (length bytes))
        bytes)
    (error (e)
      (format t "✗ FAIL: ~A~%~%" e)
      nil)))

;; Test 3: Compile eval-add helper
(format t "Test 3: Compile eval-add helper~%")
(let ((forms '(
  (defun eval-add (args)
    (+ (car args) (car (cdr args))))
  (eval-add (cons 10 (cons 5 nil))))))
  (handler-case
      (let ((bytes (compile-program-with-functions-with-runtime forms *runtime-addrs*)))
        (format t "✓ SUCCESS: eval-add compiled (~D bytes)~%~%" (length bytes))
        (format t "Expected result: 15 (0x~X tagged = ~D)~%~%" (* 15 16) (* 15 16))
        bytes)
    (error (e)
      (format t "✗ FAIL: ~A~%~%" e)
      nil)))

;; Test 4: Compile operator test
(format t "Test 4: Compile operator tests~%")
(let ((forms '(
  (defun op-is-add? (op)
    (= op 1))
  (op-is-add? 1))))
  (handler-case
      (let ((bytes (compile-program-with-functions-with-runtime forms *runtime-addrs*)))
        (format t "✓ SUCCESS: op-is-add? compiled (~D bytes)~%~%" (length bytes))
        bytes)
    (error (e)
      (format t "✗ FAIL: ~A~%~%" e)
      nil)))

;; Test 5: Full eval-expr (simplified)
(format t "Test 5: Compile simplified eval-expr~%")
(let ((forms '(
  (defun eval-simple (expr)
    (if (cons? expr)
        (let ((op (car expr)))
          (let ((arg1 (car (cdr expr))))
            (let ((arg2 (car (cdr (cdr expr)))))
              (if (= op 1)
                  (+ arg1 arg2)
                  (if (= op 2)
                      (- arg1 arg2)
                      0)))))
        expr))
  (eval-simple (cons 1 (cons 10 (cons 5 nil)))))))
  (handler-case
      (let ((bytes (compile-program-with-functions-with-runtime forms *runtime-addrs*)))
        (format t "✓ SUCCESS: eval-simple compiled (~D bytes)~%~%" (length bytes))
        (format t "Expected result: 15 (0x~X tagged = ~D)~%~%" (* 15 16) (* 15 16))
        bytes)
    (error (e)
      (format t "✗ FAIL: ~A~%~%" e)
      nil)))

(format t "~%=== Compilation Tests Complete ===~%")
