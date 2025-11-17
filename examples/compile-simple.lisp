;;;; Example: Compile simple Habu expressions to native code

(load "../bootstrap/compiler.lisp")
(load "../bootstrap/reader.lisp")
(load "../bootstrap/elf-writer.lisp")

(in-package :habu-compiler)

(format t "Habu Compiler Examples~%")
(format t "======================~%~%")

;;; Example 1: Compile a fixnum
(format t "Example 1: Compile fixnum 42~%")
(let ((code (compile-expression 42 :arch :x86_64)))
  (format t "  x86_64 code: ~{~2,'0X ~}~%" (coerce code 'list))
  (format t "  Size: ~A bytes~%~%" (length code)))

;;; Example 2: Compile addition
(format t "Example 2: Compile (+ 10 20)~%")
(let ((code (compile-expression '(+ 10 20) :arch :x86_64)))
  (format t "  x86_64 code: ~{~2,'0X ~}~%" (coerce code 'list))
  (format t "  Size: ~A bytes~%~%" (length code)))

;;; Example 3: Compile subtraction
(format t "Example 3: Compile (- 100 30)~%")
(let ((code (compile-expression '(- 100 30) :arch :x86_64)))
  (format t "  x86_64 code: ~{~2,'0X ~}~%" (coerce code 'list))
  (format t "  Size: ~A bytes~%~%" (length code)))

;;; Example 4: Compile nested arithmetic
(format t "Example 4: Compile (+ (+ 1 2) (+ 3 4))~%")
(let ((code (compile-expression '(+ (+ 1 2) (+ 3 4)) :arch :x86_64)))
  (format t "  x86_64 code size: ~A bytes~%~%" (length code)))

;;; Example 5: Compare x86_64 vs ARM64
(format t "Example 5: Compare architectures for (+ 5 10)~%")
(let ((x86-code (compile-expression '(+ 5 10) :arch :x86_64))
      (arm-code (compile-expression '(+ 5 10) :arch :arm64)))
  (format t "  x86_64: ~A bytes~%" (length x86-code))
  (format t "  ARM64:  ~A bytes~%~%" (length arm-code)))

;;; Example 6: Write to binary file
(format t "Example 6: Write compiled code to binary~%")
(compile-to-binary '(+ 15 27) "/tmp/example-add.bin" :arch :x86_64)
(format t "  Written to /tmp/example-add.bin~%")
(format t "  Result should be: ~A~%~%" (* (+ 15 27) 16)) ; Tagged as fixnum

(format t "Examples complete!~%")
