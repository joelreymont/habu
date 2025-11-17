;;;; Tests for Habu bootstrap compiler

(load "compiler.lisp")
(load "reader.lisp")
(load "elf-writer.lisp")

(in-package :habu-compiler)

(defun test-parse ()
  "Test expression parsing"
  (format t "Testing parser...~%")

  ;; Test fixnum parsing
  (let ((expr (parse 42)))
    (assert (eq (expr-type expr) 'fixnum))
    (assert (= (expr-value expr) 42))
    (format t "  Fixnum parsing: OK~%"))

  ;; Test addition parsing
  (let ((expr (parse '(+ 1 2))))
    (assert (eq (expr-type expr) 'call))
    (assert (eq (expr-value expr) '+))
    (assert (= (length (expr-args expr)) 2))
    (format t "  Addition parsing: OK~%"))

  (format t "Parser tests passed!~%~%"))

(defun test-code-gen-x86_64 ()
  "Test x86_64 code generation"
  (format t "Testing x86_64 code generation...~%")

  ;; Test simple fixnum
  (let ((code (compile-expression 42 :arch :x86_64)))
    (assert (> (length code) 0))
    (format t "  Fixnum codegen: ~A bytes~%" (length code)))

  ;; Test addition
  (let ((code (compile-expression '(+ 1 2) :arch :x86_64)))
    (assert (> (length code) 0))
    (format t "  Addition codegen: ~A bytes~%" (length code)))

  (format t "x86_64 code generation tests passed!~%~%"))

(defun test-code-gen-arm64 ()
  "Test ARM64 code generation"
  (format t "Testing ARM64 code generation...~%")

  ;; Test simple fixnum
  (let ((code (compile-expression 42 :arch :arm64)))
    (assert (> (length code) 0))
    (format t "  Fixnum codegen: ~A bytes~%" (length code)))

  ;; Test addition
  (let ((code (compile-expression '(+ 10 20) :arch :arm64)))
    (assert (> (length code) 0))
    (format t "  Addition codegen: ~A bytes~%" (length code)))

  (format t "ARM64 code generation tests passed!~%~%"))

(defun test-binary-generation ()
  "Test binary file generation"
  (format t "Testing binary generation...~%")

  ;; Generate x86_64 binary
  (let ((code (compile-expression 42 :arch :x86_64)))
    (compile-to-binary 42 "/tmp/test-x86_64.bin" :arch :x86_64)
    (format t "  x86_64 binary: /tmp/test-x86_64.bin~%"))

  ;; Generate ARM64 binary
  (let ((code (compile-expression 42 :arch :arm64)))
    (compile-to-binary 42 "/tmp/test-arm64.bin" :arch :arm64)
    (format t "  ARM64 binary: /tmp/test-arm64.bin~%"))

  (format t "Binary generation tests passed!~%~%"))

(defun test-disassemble-x86_64 ()
  "Show disassembly of generated x86_64 code"
  (format t "x86_64 code for (+ 1 2):~%")
  (let ((code (compile-expression '(+ 1 2) :arch :x86_64)))
    (format t "  Bytes: ~{~2,'0X ~}~%" (coerce code 'list))
    (format t "  Size: ~A bytes~%" (length code)))
  (format t "~%"))

(defun test-disassemble-arm64 ()
  "Show disassembly of generated ARM64 code"
  (format t "ARM64 code for (+ 1 2):~%")
  (let ((code (compile-expression '(+ 1 2) :arch :arm64)))
    (format t "  Bytes: ~{~2,'0X ~}~%" (coerce code 'list))
    (format t "  Size: ~A bytes~%" (length code)))
  (format t "~%"))

(defun run-all-tests ()
  "Run all compiler tests"
  (format t "~%Habu Bootstrap Compiler Tests~%")
  (format t "==============================~%~%")

  (test-parse)
  (test-code-gen-x86_64)
  (test-code-gen-arm64)
  (test-binary-generation)
  (test-disassemble-x86_64)
  (test-disassemble-arm64)

  (format t "All tests passed!~%")
  t)

;; Run tests when loaded
(run-all-tests)
