;;; Test file I/O primitives in bootstrap compiler
(load "bootstrap/compiler.lisp")
(in-package :habu)

(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

(defun test-eval (name source expected)
  (handler-case
    (let* ((forms (nc-read-all source))
           (result (nc-eval-forms forms)))
      (if (equal result expected)
          (progn
            (format t "[PASS] ~A~%" name)
            (incf *tests-passed*))
          (progn
            (format t "[FAIL] ~A: expected ~S, got ~S~%" name expected result)
            (incf *tests-failed*))))
    (error (e)
      (format t "[FAIL] ~A: error ~A~%" name e)
      (incf *tests-failed*))))

(format t "~%=== Bootstrap File I/O Tests ===~%~%")

;; Test write-file and read-file round-trip
(let ((test-file "/tmp/habu_test_io.txt")
      (test-content "Hello from Habu!"))
  ;; Write
  (test-eval "write-file"
    (format nil "(write-file ~S ~S)" test-file test-content)
    test-content)

  ;; Read back
  (test-eval "read-file"
    (format nil "(read-file ~S)" test-file)
    test-content)

  ;; Clean up
  (when (probe-file test-file)
    (delete-file test-file)))

;; Test println (returns the value printed)
(test-eval "println-number"
  "(println 42)"
  42)

(test-eval "println-string"
  "(println \"test\")"
  "test")

;; Test string-length
(test-eval "string-length-empty"
  "(string-length \"\")"
  0)

(test-eval "string-length-simple"
  "(string-length \"hello\")"
  5)

(test-eval "string-length-unicode"
  "(string-length \"abc123\")"
  6)

;; Test string-ref
(test-eval "string-ref-first"
  "(string-ref \"hello\" 0)"
  (char-code #\h))

(test-eval "string-ref-middle"
  "(string-ref \"hello\" 2)"
  (char-code #\l))

(test-eval "string-ref-last"
  "(string-ref \"hello\" 4)"
  (char-code #\o))

;; Test string= comparison
(test-eval "string=-equal"
  "(string= \"hello\" \"hello\")"
  1)

(test-eval "string=-different"
  "(string= \"hello\" \"world\")"
  0)

(test-eval "string=-diff-length"
  "(string= \"hi\" \"hello\")"
  0)

(test-eval "string=-empty"
  "(string= \"\" \"\")"
  1)

(format t "~%=== Results: ~A passed, ~A failed ===~%" *tests-passed* *tests-failed*)
(when (> *tests-failed* 0)
  (sb-ext:exit :code 1))
