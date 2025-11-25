#!/usr/bin/env sbcl --script
;;; Test reader integration - using Habu reader to read and compile code

(load "run-habu.lisp")

(defparameter *tests-passed* 0)
(defparameter *tests-failed* 0)

(defun load-reader-forms ()
  "Load reader.lisp and return its forms as a list"
  (with-open-file (in "common/reader.lisp")
    (let ((forms nil))
      (handler-case
          (loop
            (let ((form (read in nil :eof)))
              (if (eq form :eof)
                  (return (nreverse forms))
                  (push form forms))))
        (end-of-file () (nreverse forms))))))

(defun run-test (name forms expected)
  "Run a reader integration test"
  (multiple-value-bind (result output)
      (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (eql result expected)
        (progn
          (format t "[PASS] ~A~%" name)
          (incf *tests-passed*))
        (progn
          (format t "[FAIL] ~A - got ~A, expected ~A~%" name result expected)
          (incf *tests-failed*)))))

(defparameter *reader-forms* (load-reader-forms))

(format t "~%=== Reader-Compiler Integration Tests ===~%~%")

;; Test 1: Read arithmetic expression and extract parts
(run-test "read-expr-operator"
  (append *reader-forms*
          '((let ((expr (read-from-string "(+ 1 2)")))
              (if (eq (car expr) '+) #x1 #x0))))
  #x1)

;; Test 2: Read defun and extract function name
(run-test "read-defun-name"
  (append *reader-forms*
          '((let ((form (read-from-string "(defun foo (x) (+ x 1))")))
              (if (eq (cadr form) 'foo) #x1 #x0))))
  #x1)

;; Test 3: Read defun and extract params
(run-test "read-defun-params-length"
  (append *reader-forms*
          '((let ((form (read-from-string "(defun bar (a b c) (+ a b c))")))
              (length (caddr form)))))
  #x3)

;; Test 4: Read nested expression
(run-test "read-nested-expr"
  (append *reader-forms*
          '((let ((form (read-from-string "((a b) (c d))")))
              (+ (length (car form)) (length (cadr form))))))
  #x4)

;; Test 5: Read quote form
(run-test "read-quote-form"
  (append *reader-forms*
          '((let ((form (read-from-string "'(1 2 3)")))
              (if (eq (car form) 'quote) #x1 #x0))))
  #x1)

;; Test 6: Read hex literal
(run-test "read-hex-literal"
  (append *reader-forms*
          '((read-from-string "#xFF")))
  #xFF)

;; Test 7: Read string content
(run-test "read-string-length"
  (append *reader-forms*
          '((string-length (read-from-string "\"hello\""))))
  #x5)

;; Test 8: Read multiple defuns and count
(run-test "read-multiple-defuns"
  (append *reader-forms*
          '((length (read-all-from-string "(defun a () 1) (defun b () 2) (defun c () 3)"))))
  #x3)

;; Test 9: Read and verify symbol interning
(run-test "read-symbol-eq"
  (append *reader-forms*
          '((let ((forms (read-all-from-string "(foo) (foo)")))
              ;; Both should be the same interned symbol
              (if (eq (car (car forms)) (car (cadr forms))) #x1 #x0))))
  #x1)

;; Test 10: Read let binding form
(run-test "read-let-bindings"
  (append *reader-forms*
          '((let ((form (read-from-string "(let ((x 1) (y 2)) (+ x y))")))
              (length (cadr form)))))  ; number of bindings
  #x2)

;; Summary
(format t "~%=== Results: ~A passed, ~A failed ===~%"
        *tests-passed* *tests-failed*)

(if (> *tests-failed* 0)
    (sb-ext:exit :code 1)
    (sb-ext:exit :code 0))
