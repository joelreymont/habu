;;;; Test hash tables

(load "compiler.lisp")
(in-package :habu-compiler)

(initialize-runtime-integration)

(format t "~%Testing Hash Tables~%")
(format t "===================~%~%")

(defvar *test-count* 0)
(defvar *pass-count* 0)

(defun test (name condition &optional message)
  (incf *test-count*)
  (if condition
      (progn
        (incf *pass-count*)
        (format t "[32m✓[0m ~A~%"name))
      (progn
        (format t "[31m✗[0m ~A" name)
        (when message
          (format t ": ~A" message))
        (format t "~%"))))

;; Test 1: make-hash-table compilation
(format t "~%[34m1. Test make-hash-table[0m~%")
(format t "========================~%")

(handler-case
    (let ((code (compile-expression '(make-hash-table) :arch :x86_64)))
      (test "MAKE-HASH-TABLE-COMPILES" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "MAKE-HASH-TABLE-COMPILES" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(make-hash-table 32) :arch :x86_64)))
      (test "MAKE-HASH-TABLE-WITH-CAPACITY" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "MAKE-HASH-TABLE-WITH-CAPACITY" nil (format nil "~A" e))))

;; Test 2: gethash compilation
(format t "~%[34m2. Test gethash[0m~%")
(format t "==================~%")

(handler-case
    (let ((code (compile-expression '(let ((ht (make-hash-table)))
                                       (gethash 1 ht))
                                    :arch :x86_64)))
      (test "GETHASH-COMPILES" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "GETHASH-COMPILES" nil (format nil "~A" e))))

;; Test 3: puthash compilation
(format t "~%[34m3. Test puthash[0m~%")
(format t "==================~%")

(handler-case
    (let ((code (compile-expression '(let ((ht (make-hash-table)))
                                       (puthash 1 42 ht))
                                    :arch :x86_64)))
      (test "PUTHASH-COMPILES" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "PUTHASH-COMPILES" nil (format nil "~A" e))))

;; Test 4: remhash compilation
(format t "~%[34m4. Test remhash[0m~%")
(format t "==================~%")

(handler-case
    (let ((code (compile-expression '(let ((ht (make-hash-table)))
                                       (remhash 1 ht))
                                    :arch :x86_64)))
      (test "REMHASH-COMPILES" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "REMHASH-COMPILES" nil (format nil "~A" e))))

;; Test 5: hash-table-count compilation
(format t "~%[34m5. Test hash-table-count[0m~%")
(format t "===========================~%")

(handler-case
    (let ((code (compile-expression '(let ((ht (make-hash-table)))
                                       (hash-table-count ht))
                                    :arch :x86_64)))
      (test "HASH-TABLE-COUNT-COMPILES" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "HASH-TABLE-COUNT-COMPILES" nil (format nil "~A" e))))

;; Test 6: ARM64 versions
(format t "~%[34m6. Test ARM64 Hash Tables[0m~%")
(format t "============================~%")

(handler-case
    (let ((code (compile-expression '(make-hash-table) :arch :arm64)))
      (test "ARM64-MAKE-HASH-TABLE" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "ARM64-MAKE-HASH-TABLE" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(let ((ht (make-hash-table)))
                                       (gethash 1 ht))
                                    :arch :arm64)))
      (test "ARM64-GETHASH" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "ARM64-GETHASH" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(let ((ht (make-hash-table)))
                                       (puthash 1 42 ht))
                                    :arch :arm64)))
      (test "ARM64-PUTHASH" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "ARM64-PUTHASH" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(let ((ht (make-hash-table)))
                                       (remhash 1 ht))
                                    :arch :arm64)))
      (test "ARM64-REMHASH" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "ARM64-REMHASH" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(let ((ht (make-hash-table)))
                                       (hash-table-count ht))
                                    :arch :arm64)))
      (test "ARM64-HASH-TABLE-COUNT" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "ARM64-HASH-TABLE-COUNT" nil (format nil "~A" e))))

;; Test 7: Complex expressions
(format t "~%[34m7. Test Complex Hash Table Expressions[0m~%")
(format t "=========================================~%")

(handler-case
    (let ((code (compile-expression '(let ((ht (make-hash-table)))
                                       (puthash 1 100 ht)
                                       (puthash 2 200 ht)
                                       (gethash 1 ht))
                                    :arch :x86_64)))
      (test "COMPLEX-HASH-EXPRESSION" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "COMPLEX-HASH-EXPRESSION" nil (format nil "~A" e))))

;; Summary
(format t "~%=====================================~%")
(format t "  Test Results~%")
(format t "=====================================~%")
(format t "Total:  ~D~%"  *test-count*)
(format t "Passed: [32m~D/~D[0m~%~%" *pass-count* *test-count*)

(when (< *pass-count* *test-count*)
  (format t "[31mSome tests failed![0m~%")
  (sb-ext:quit :unix-status 1))

(format t "[32mAll hash table tests passed![0m~%")

(sb-ext:quit)
