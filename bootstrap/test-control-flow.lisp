;;;; Test control flow constructs (dotimes, dolist, block/return-from)

(load "compiler.lisp")
(in-package :habu-compiler)

(initialize-runtime-integration)

(format t "~%Testing Control Flow Constructs~%")
(format t "================================~%~%")

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

;; Test 1: dotimes compilation
(format t "~%[34m1. Test dotimes[0m~%")
(format t "==================~%")

(handler-case
    (let ((code (compile-expression '(dotimes (i 5) (+ i 1)) :arch :x86_64)))
      (test "DOTIMES-COMPILES" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "DOTIMES-COMPILES" nil (format nil "~A" e))))

;; Test 2: dotimes with result
(handler-case
    (let ((code (compile-expression '(dotimes (i 10 (* i 2)) (print i)) :arch :x86_64)))
      (test "DOTIMES-WITH-RESULT" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "DOTIMES-WITH-RESULT" nil (format nil "~A" e))))

;; Test 3: dolist compilation
(format t "~%[34m2. Test dolist[0m~%")
(format t "=================~%")

(handler-case
    (let ((code (compile-expression '(dolist (x (list 1 2 3)) (print x)) :arch :x86_64)))
      (test "DOLIST-COMPILES" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "DOLIST-COMPILES" nil (format nil "~A" e))))

;; Test 4: dolist with result
(handler-case
    (let ((code (compile-expression '(dolist (x mylist 42) (+ x 1)) :arch :x86_64)))
      (test "DOLIST-WITH-RESULT" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "DOLIST-WITH-RESULT" nil (format nil "~A" e))))

;; Test 5: block/return-from compilation
(format t "~%[34m3. Test block/return-from[0m~%")
(format t "===========================~%")

(handler-case
    (let ((code (compile-expression '(block search (+ 1 2)) :arch :x86_64)))
      (test "BLOCK-COMPILES" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "BLOCK-COMPILES" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(block search
                                       (dotimes (i 10)
                                         (if (= i 5)
                                             (return-from search i)))
                                       99)
                                    :arch :x86_64)))
      (test "RETURN-FROM-COMPILES" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "RETURN-FROM-COMPILES" nil (format nil "~A" e))))

;; Test 6: ARM64 versions
(format t "~%[34m4. Test ARM64 Control Flow[0m~%")
(format t "============================~%")

(handler-case
    (let ((code (compile-expression '(dotimes (i 5) (+ i 1)) :arch :arm64)))
      (test "ARM64-DOTIMES" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "ARM64-DOTIMES" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(dolist (x (list 1 2 3)) x) :arch :arm64)))
      (test "ARM64-DOLIST" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "ARM64-DOLIST" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(block done (return-from done 42)) :arch :arm64)))
      (test "ARM64-BLOCK-RETURN" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "ARM64-BLOCK-RETURN" nil (format nil "~A" e))))

;; Summary
(format t "~%=====================================~%")
(format t "  Test Results~%")
(format t "=====================================~%")
(format t "Total:  ~D~%"  *test-count*)
(format t "Passed: [32m~D/~D[0m~%~%" *pass-count* *test-count*)

(when (< *pass-count* *test-count*)
  (format t "[31mSome tests failed![0m~%")
  (sb-ext:quit :unix-status 1))

(format t "[32mAll control flow tests passed![0m~%")

(sb-ext:quit)
