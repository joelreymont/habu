;;;; Test string literal compilation

(load "compiler.lisp")
(in-package :habu-compiler)

(initialize-runtime-integration)

(format t "~%Testing String Literals~%")
(format t "======================~%~%")

(defvar *test-count* 0)
(defvar *pass-count* 0)

(defun test (name condition &optional message)
  (incf *test-count*)
  (if condition
      (progn
        (incf *pass-count*)
        (format t "[32m✓[0m ~A~%" name))
      (progn
        (format t "[31m✗[0m ~A" name)
        (when message
          (format t ": ~A" message))
        (format t "~%"))))

;; Test 1: Simple string literal compilation (x86_64)
(format t "~%[34m1. String Literal Compilation (x86_64)[0m~%")
(format t "==========================================~%")

(handler-case
    (let ((code (compile-expression "hello world" :arch :x86_64)))
      (test "STRING-LITERAL-X86_64" (> (length code) 0))
      (format t "   Generated ~D bytes~%" (length code)))
  (error (e)
    (test "STRING-LITERAL-X86_64" nil (format nil "~A" e))))

;; Test 2: String literal with special characters
(format t "~%[34m2. String with Special Characters[0m~%")
(format t "====================================~%")

(handler-case
    (let ((code (compile-expression "hello\nworld\ttab" :arch :x86_64)))
      (test "STRING-SPECIAL-CHARS" (> (length code) 0))
      (format t "   Generated ~D bytes~%" (length code)))
  (error (e)
    (test "STRING-SPECIAL-CHARS" nil (format nil "~A" e))))

;; Test 3: Empty string
(format t "~%[34m3. Empty String[0m~%")
(format t "==================~%")

(handler-case
    (let ((code (compile-expression "" :arch :x86_64)))
      (test "EMPTY-STRING" (> (length code) 0))
      (format t "   Generated ~D bytes~%" (length code)))
  (error (e)
    (test "EMPTY-STRING" nil (format nil "~A" e))))

;; Test 4: ARM64 string literal
(format t "~%[34m4. String Literal Compilation (ARM64)[0m~%")
(format t "==========================================~%")

(handler-case
    (let ((code (compile-expression "hello" :arch :arm64)))
      (test "STRING-LITERAL-ARM64" (> (length code) 0))
      (format t "   Generated ~D bytes~%" (length code)))
  (error (e)
    (test "STRING-LITERAL-ARM64" nil (format nil "~A" e))))

;; Test 5: Unicode string (known limitation - ASCII only for now)
(format t "~%[34m5. Unicode String (Expected Limitation)[0m~%")
(format t "==========================================~%")

(handler-case
    (let ((code (compile-expression "hello 世界" :arch :x86_64)))
      (test "UNICODE-STRING" nil "Unicode should not be supported yet (ASCII only)"))
  (error (e)
    (test "UNICODE-STRING-LIMITATION" t)
    (format t "   Expected: ASCII-only (no UTF-8 yet)~%")))

;; Summary
(format t "~%=====================================~%")
(format t "  Test Results~%")
(format t "=====================================~%")
(format t "Total:  ~D~%"*test-count*)
(format t "Passed: [32m~D/~D[0m~%~%" *pass-count* *test-count*)

(when (< *pass-count* *test-count*)
  (format t "[31mSome tests failed![0m~%")
  (sb-ext:quit :unix-status 1))

(format t "[32mAll string literal tests passed![0m~%")

(sb-ext:quit)
