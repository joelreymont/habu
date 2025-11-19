;;;; Test extended list operations

(load "compiler.lisp")
(in-package :habu-compiler)

(initialize-runtime-integration)

(format t "~%Testing Extended List Operations~%")
(format t "==================================~%~%")

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

;; Test 1: butlast compilation
(format t "~%[34m1. Test butlast[0m~%")
(format t "===================~%")

(handler-case
    (let ((code (compile-expression '(butlast (list 1 2 3)) :arch :x86_64)))
      (test "BUTLAST-COMPILES" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "BUTLAST-COMPILES" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(butlast (list 1 2 3 4) 2) :arch :x86_64)))
      (test "BUTLAST-WITH-N" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "BUTLAST-WITH-N" nil (format nil "~A" e))))

;; Test 2: nthcdr compilation
(format t "~%[34m2. Test nthcdr[0m~%")
(format t "==================~%")

(handler-case
    (let ((code (compile-expression '(nthcdr 2 (list 1 2 3 4)) :arch :x86_64)))
      (test "NTHCDR-COMPILES" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "NTHCDR-COMPILES" nil (format nil "~A" e))))

;; Test 3: member compilation
(format t "~%[34m3. Test member[0m~%")
(format t "==================~%")

(handler-case
    (let ((code (compile-expression '(member 2 (list 1 2 3)) :arch :x86_64)))
      (test "MEMBER-COMPILES" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "MEMBER-COMPILES" nil (format nil "~A" e))))

;; Test 4: assoc compilation
(format t "~%[34m4. Test assoc[0m~%")
(format t "=================~%")

(handler-case
    (let ((code (compile-expression '(assoc 2 (list (cons 1 10) (cons 2 20) (cons 3 30))) :arch :x86_64)))
      (test "ASSOC-COMPILES" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "ASSOC-COMPILES" nil (format nil "~A" e))))

;; Test 5: position compilation
(format t "~%[34m5. Test position[0m~%")
(format t "====================~%")

(handler-case
    (let ((code (compile-expression '(position 3 (list 1 2 3 4)) :arch :x86_64)))
      (test "POSITION-COMPILES" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "POSITION-COMPILES" nil (format nil "~A" e))))

;; Test 6: count compilation
(format t "~%[34m6. Test count[0m~%")
(format t "=================~%")

(handler-case
    (let ((code (compile-expression '(count 2 (list 1 2 3 2 4)) :arch :x86_64)))
      (test "COUNT-COMPILES" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "COUNT-COMPILES" nil (format nil "~A" e))))

;; Test 7: remove compilation
(format t "~%[34m7. Test remove[0m~%")
(format t "==================~%")

(handler-case
    (let ((code (compile-expression '(remove 2 (list 1 2 3 2 4)) :arch :x86_64)))
      (test "REMOVE-COMPILES" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "REMOVE-COMPILES" nil (format nil "~A" e))))

;; Test 8: ARM64 versions
(format t "~%[34m8. Test ARM64 List Operations[0m~%")
(format t "=================================~%")

(handler-case
    (let ((code (compile-expression '(butlast (list 1 2 3)) :arch :arm64)))
      (test "ARM64-BUTLAST" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "ARM64-BUTLAST" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(nthcdr 2 (list 1 2 3 4)) :arch :arm64)))
      (test "ARM64-NTHCDR" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "ARM64-NTHCDR" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(member 2 (list 1 2 3)) :arch :arm64)))
      (test "ARM64-MEMBER" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "ARM64-MEMBER" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(assoc 2 (list (cons 1 10) (cons 2 20))) :arch :arm64)))
      (test "ARM64-ASSOC" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "ARM64-ASSOC" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(position 3 (list 1 2 3 4)) :arch :arm64)))
      (test "ARM64-POSITION" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "ARM64-POSITION" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(count 2 (list 1 2 3 2 4)) :arch :arm64)))
      (test "ARM64-COUNT" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "ARM64-COUNT" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(remove 2 (list 1 2 3 2 4)) :arch :arm64)))
      (test "ARM64-REMOVE" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "ARM64-REMOVE" nil (format nil "~A" e))))

;; Test 9: Complex expressions
(format t "~%[34m9. Test Complex List Expressions[0m~%")
(format t "====================================~%")

(handler-case
    (let ((code (compile-expression '(let ((lst (list 1 2 3 4 5)))
                                       (butlast lst 2))
                                    :arch :x86_64)))
      (test "COMPLEX-BUTLAST" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "COMPLEX-BUTLAST" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(let ((lst (list 1 2 3 4 5)))
                                       (member 3 (nthcdr 1 lst)))
                                    :arch :x86_64)))
      (test "COMPLEX-MEMBER-NTHCDR" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "COMPLEX-MEMBER-NTHCDR" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(let ((alist (list (cons 1 10) (cons 2 20) (cons 3 30))))
                                       (assoc 2 alist))
                                    :arch :x86_64)))
      (test "COMPLEX-ASSOC" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "COMPLEX-ASSOC" nil (format nil "~A" e))))

;; Summary
(format t "~%=====================================~%")
(format t "  Test Results~%")
(format t "=====================================~%")
(format t "Total:  ~D~%"  *test-count*)
(format t "Passed: [32m~D/~D[0m~%~%" *pass-count* *test-count*)

(when (< *pass-count* *test-count*)
  (format t "[31mSome tests failed![0m~%")
  (sb-ext:quit :unix-status 1))

(format t "[32mAll list operation tests passed![0m~%")

(sb-ext:quit)
