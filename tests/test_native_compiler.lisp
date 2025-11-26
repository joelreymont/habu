#!/usr/bin/env sbcl --script
;;; Test native compiler delivery and execution

(load "run-habu.lisp")

(defparameter *tests-passed* 0)
(defparameter *tests-failed* 0)

(defun run-nc-test (name source expected)
  "Deliver native compiler with given source, run, check exit code"
  ;; Read native-compiler.lisp
  (let* ((nc-source (with-open-file (in "native-compiler.lisp")
                      (let ((str (make-string (file-length in))))
                        (read-sequence str in)
                        str)))
         ;; Find the line with src assignment and replace the whole expression
         (pattern "(+ (* 3 4) 5)")
         (start (search pattern nc-source)))
    (if start
        (let ((patched (concatenate 'string
                                    (subseq nc-source 0 start)
                                    source
                                    (subseq nc-source (+ start (length pattern))))))
          ;; Write patched source to temp file
          (with-open-file (out "/tmp/nc-test.lisp" :direction :output :if-exists :supersede)
            (write-string patched out))
          ;; Deliver
          (habu-sbcl:habu-deliver "/tmp/nc-test.lisp" "/tmp/nc-test" :verbose nil)
          ;; Run and check
          (let ((exit-code (sb-ext:process-exit-code
                            (sb-ext:run-program "/tmp/nc-test" nil :output nil :error nil :wait t))))
            (if (= exit-code expected)
                (progn
                  (format t "[PASS] ~A: ~A = ~A~%" name source expected)
                  (incf *tests-passed*))
                (progn
                  (format t "[FAIL] ~A: ~A expected ~A got ~A~%" name source expected exit-code)
                  (incf *tests-failed*)))))
        (format t "[ERROR] Could not find pattern to replace~%"))))

(format t "~%=== Native Compiler Tests ===~%~%")

;; Test 1: Simple addition
(run-nc-test "add" "(+ 10 7)" 17)

;; Test 2: Simple multiplication
(run-nc-test "mul" "(* 3 4)" 12)

;; Test 3: Nested arithmetic
(run-nc-test "nested" "(+ (* 3 4) 5)" 17)

;; Test 4: Subtraction
(run-nc-test "sub" "(- 20 8)" 12)

;; Test 5: Complex expression
(run-nc-test "complex" "(+ (- 100 50) (* 2 10))" 70)

;; Test 6: Let binding
(run-nc-test "let" "(let ((x 5)) (+ x 3))" 8)

;; Test 7: Let with multiple bindings
(run-nc-test "let-multi" "(let ((a 3) (b 4)) (* a b))" 12)

;; Test 8: Comparison equals
(run-nc-test "cmp-eq" "(if (= 5 5) 1 0)" 1)

;; Test 9: Comparison less than
(run-nc-test "cmp-lt" "(if (< 3 5) 10 20)" 10)

;; Test 10: Nested let
(run-nc-test "let-nested" "(let ((x 2)) (let ((y 3)) (* x y)))" 6)

;; Summary
(format t "~%=== Results: ~A passed, ~A failed ===~%"
        *tests-passed* *tests-failed*)

(if (> *tests-failed* 0)
    (sb-ext:exit :code 1)
    (sb-ext:exit :code 0))
