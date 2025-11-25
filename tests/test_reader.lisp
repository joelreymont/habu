;;; Test Habu Reader - verifies reader parses Lisp source correctly
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
  "Run a reader test and check result"
  (multiple-value-bind (result output)
      (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (eql result expected)
        (progn
          (format t "[PASS] ~A~%" name)
          (incf *tests-passed*))
        (progn
          (format t "[FAIL] ~A - got ~X, expected ~X~%" name result expected)
          (incf *tests-failed*)))))

;; Load reader forms once
(defparameter *reader-forms* (load-reader-forms))

(format t "~%=== Habu Reader Tests ===~%~%")

;; Test 1: Read integer
(run-test "read-integer-42"
  (append *reader-forms*
          '((read-from-string "42")))
  #x2A)

;; Test 2: Read negative integer
(run-test "read-negative-10"
  (append *reader-forms*
          '((read-from-string "-10")))
  -10)

;; Test 3: Read hex lowercase
(run-test "read-hex-ff"
  (append *reader-forms*
          '((read-from-string "#xff")))
  #xFF)

;; Test 4: Read hex uppercase
(run-test "read-hex-FF"
  (append *reader-forms*
          '((read-from-string "#XFF")))
  #xFF)

;; Test 5: Read list first element
(run-test "read-list-car"
  (append *reader-forms*
          '((car (read-from-string "(1 2 3)"))))
  #x1)

;; Test 6: Read list length
(run-test "read-list-length"
  (append *reader-forms*
          '((length (read-from-string "(1 2 3)"))))
  #x3)

;; Test 7: Read nested list
(run-test "read-nested-list"
  (append *reader-forms*
          '((car (car (read-from-string "((1 2) 3)")))))
  #x1)

;; Test 8: Read empty list
(run-test "read-empty-list"
  (append *reader-forms*
          '((if (null (read-from-string "()")) #x1 #x0)))
  #x1)

;; Test 9: Read symbol is symbol
(run-test "read-symbol"
  (append *reader-forms*
          '((if (symbolp (read-from-string "foo")) #x1 #x0)))
  #x1)

;; Test 10: Read t
(run-test "read-t"
  (append *reader-forms*
          '((if (eq (read-from-string "t") t) #x1 #x0)))
  #x1)

;; Test 11: Read nil
(run-test "read-nil"
  (append *reader-forms*
          '((if (null (read-from-string "nil")) #x1 #x0)))
  #x1)

;; Test 12: Read string is string
(run-test "read-string"
  (append *reader-forms*
          '((if (stringp (read-from-string "\"hello\"")) #x1 #x0)))
  #x1)

;; Test 13: Read quote
(run-test "read-quote"
  (append *reader-forms*
          '((if (eq (car (read-from-string "'x")) 'quote) #x1 #x0)))
  #x1)

;; Test 14: Read character literal
(run-test "read-char-literal"
  (append *reader-forms*
          '((read-from-string "#\\A")))
  #x41)

;; Test 15: Read named character
(run-test "read-newline"
  (append *reader-forms*
          '((read-from-string "#\\newline")))
  #x0A)

;; Test 16: Read function quote
(run-test "read-function-quote"
  (append *reader-forms*
          '((if (eq (car (read-from-string "#'foo")) 'function) #x1 #x0)))
  #x1)

;; Test 17: Skip whitespace
(run-test "skip-whitespace"
  (append *reader-forms*
          '((read-from-string "   42")))
  #x2A)

;; Test 18: Skip comment
(run-test "skip-comment"
  (append *reader-forms*
          '((read-from-string "; comment
42")))
  #x2A)

;; Test 19: Read backquote
(run-test "read-backquote"
  (append *reader-forms*
          '((if (eq (car (read-from-string "`x")) 'quasiquote) #x1 #x0)))
  #x1)

;; Test 20: Read comma
(run-test "read-unquote"
  (append *reader-forms*
          '((if (eq (car (read-from-string ",x")) 'unquote) #x1 #x0)))
  #x1)

;; Test 21: Read improper list
(run-test "read-improper-list"
  (append *reader-forms*
          '((cdr (read-from-string "(1 . 2)"))))
  #x2)

;; Test 22: Read all from string
(run-test "read-all-count"
  (append *reader-forms*
          '((length (read-all-from-string "1 2 3"))))
  #x3)

;; Summary
(format t "~%=== Results: ~A passed, ~A failed ===~%"
        *tests-passed* *tests-failed*)

(if (> *tests-failed* 0)
    (sb-ext:exit :code 1)
    (sb-ext:exit :code 0))
