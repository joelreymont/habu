;;;; Compile factorial function and dump bytecode for testing
;;;; Generates C-compatible byte array for test-factorial-jit.c

(load "run-habu.lisp")

(in-package :habu-sbcl-codegen)

(defun dump-bytes-as-c-array (bytes name)
  "Dump byte list as C array initializer"
  (format t "uint8_t ~A[] = {~%" name)
  (format t "    ")
  (loop for b in bytes
        for i from 0
        do (progn
             (when (and (> i 0) (zerop (mod i 12)))
               (format t "~%    "))
             (format t "0x~2,'0X" b)
             (when (< i (1- (length bytes)))
               (format t ", "))))
  (format t "~%};~%")
  (format t "size_t ~A_size = ~D;~%~%" name (length bytes)))

(format t "~%/* Factorial function compiled to ARM64 */~%")
(format t "/* (defun factorial (n) (if (= n 0) 1 (* n (factorial (- n 1))))) */~%~%")

;; Compile factorial function
(let ((forms '(
  (defun factorial (n)
    (if (= n 0)
        1
        (* n (factorial (- n 1)))))
  (factorial 5))))
  (handler-case
      (let ((bytes (compile-program-with-functions-with-runtime forms *runtime-addrs*)))
        (format t "/* Generated ~D bytes */~%~%" (length bytes))
        (dump-bytes-as-c-array bytes "factorial_code")
        (format t "/* Expected result: factorial(5) = 120 (0x~X tagged = ~D) */~%"
                (* 120 16) (* 120 16)))
    (error (e)
      (format t "/* Compilation error: ~A */~%" e))))
