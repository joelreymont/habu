; Script to compile Lisp expressions and dump bytecode
; Uses habu-arm64-codegen.lisp to generate real code

(load "run-habu.lisp")

(in-package :habu-sbcl-codegen)

(defun dump-bytes-as-c-array (bytes name)
  "Dump byte list as C array initializer"
  (format t "    uint8_t ~A[] = {~%" name)
  (format t "        ")
  (loop for b in bytes
        for i from 0
        do (progn
             (when (and (> i 0) (zerop (mod i 12)))
               (format t "~%        "))
             (format t "0x~2,'0X" b)
             (when (< i (1- (length bytes)))
               (format t ", "))))
  (format t "~%    };~%")
  (format t "    /* Size: ~D bytes */~%~%" (length bytes)))

(defun test-compile-expression (expr)
  "Compile an expression and show the bytecode"
  (format t "~%Compiling: ~S~%" expr)
  (handler-case
      (let ((bytes (compile-to-arm64-with-runtime expr *runtime-addrs*)))
        (format t "Generated ~D bytes~%" (length bytes))
        (dump-bytes-as-c-array bytes "code")
        bytes)
    (error (e)
      (format t "Compilation error: ~A~%" e)
      nil)))

(format t "~%=== Habu Compiler Bytecode Dump ===~%")

; Test simple expressions
(format t "~%--- Simple Literals ---~%")
(test-compile-expression 42)

(format t "~%--- Arithmetic ---~%")
(test-compile-expression '(+ 21 21))
(test-compile-expression '(* 6 7))

(format t "~%--- Nested Arithmetic ---~%")
(test-compile-expression '(+ (* 2 3) (* 4 5)))

(format t "~%Done~%")
