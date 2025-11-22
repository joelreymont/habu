#!/usr/bin/env sbcl --script
;;; test-comparisons.lisp - Test all comparison operators

(load "sbcl-habu-shim.lisp")
(load "habu-arm64-codegen-sbcl.lisp")

(defun write-bytecode-to-file (code-list filename)
  "Write bytecode (list of bytes) to binary file"
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create
                       :element-type '(unsigned-byte 8))
    (dolist (byte code-list)
      (write-byte byte out))))

(defun test-comparison (expr expected-result)
  "Test a comparison expression"
  (let* ((ir (habu-sbcl-codegen:compile-expr expr nil nil))
         (code (habu-sbcl-codegen:compile-to-arm64 expr))
         (filename "/tmp/test-comp.bin"))
    (format t "Testing: ~S~%" expr)
    (format t "  IR: ~S~%" ir)
    (format t "  Code size: ~D bytes~%" (length code))

    ;; Write and execute
    (write-bytecode-to-file code filename)
    (let* ((output (with-output-to-string (stream)
                     (sb-ext:run-program "./run-bytecode" (list filename)
                                        :output stream :search t)))
           (lines (loop for i = 0 then (1+ j)
                       as j = (position #\Newline output :start i)
                       collect (subseq output i j)
                       while j))
           ;; Parse result from "Untagged fixnum: N" line
           (result-line (find "Untagged fixnum:" lines
                             :test (lambda (x y) (search x y))))
           (result (when result-line
                    (parse-integer result-line
                                  :start (+ 17 (search "Untagged fixnum:" result-line))))))
      (format t "  Result: ~D (expected ~D) ~A~%~%"
              result expected-result
              (if (= result expected-result) "✓" "✗"))
      (= result expected-result))))

;; Test suite
(defun run-tests ()
  (format t "=== Comparison Operator Tests ===~%~%")

  (let ((passed 0)
        (total 0))

    ;; Equality tests
    (when (test-comparison '(= 5 5) 1) (incf passed)) (incf total)
    (when (test-comparison '(= 5 7) 0) (incf passed)) (incf total)

    ;; Less than tests
    (when (test-comparison '(< 5 7) 1) (incf passed)) (incf total)
    (when (test-comparison '(< 7 5) 0) (incf passed)) (incf total)
    (when (test-comparison '(< 5 5) 0) (incf passed)) (incf total)

    ;; Greater than tests
    (when (test-comparison '(> 7 5) 1) (incf passed)) (incf total)
    (when (test-comparison '(> 5 7) 0) (incf passed)) (incf total)
    (when (test-comparison '(> 5 5) 0) (incf passed)) (incf total)

    ;; Less than or equal tests
    (when (test-comparison '(<= 5 7) 1) (incf passed)) (incf total)
    (when (test-comparison '(<= 5 5) 1) (incf passed)) (incf total)
    (when (test-comparison '(<= 7 5) 0) (incf passed)) (incf total)

    ;; Greater than or equal tests
    (when (test-comparison '(>= 7 5) 1) (incf passed)) (incf total)
    (when (test-comparison '(>= 5 5) 1) (incf passed)) (incf total)
    (when (test-comparison '(>= 5 7) 0) (incf passed)) (incf total)

    ;; Not equal tests (using standard Lisp /= operator)
    (when (test-comparison '(/= 5 7) 1) (incf passed)) (incf total)
    (when (test-comparison '(/= 5 5) 0) (incf passed)) (incf total)

    ;; Complex tests with nested comparisons
    (when (test-comparison '(if (< 3 5) 100 200) 100) (incf passed)) (incf total)
    (when (test-comparison '(if (> 3 5) 100 200) 200) (incf passed)) (incf total)
    (when (test-comparison '(if (<= 5 5) 42 99) 42) (incf passed)) (incf total)

    (format t "=== Summary ===~%")
    (format t "Passed: ~D/~D tests~%" passed total)
    (if (= passed total)
        (format t "✓ All tests passed!~%")
        (format t "✗ Some tests failed~%"))

    (= passed total)))

;; Run the tests
(if (run-tests)
    (sb-ext:quit :unix-status 0)
    (sb-ext:quit :unix-status 1))