#!/usr/bin/env sbcl --script
;;; test-defun.lisp - Test function definition and calls

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

(defun test-defun-expr (forms expected-result description)
  "Test forms including defun and function calls"
  (format t "~A~%" description)
  (format t "  Forms: ~S~%" forms)

  (let* ((code (habu-sbcl-codegen:compile-program-with-functions forms))
         (filename "/tmp/test-defun.bin"))
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
           ;; Parse result from "Untagged fixnum:" line
           (result-line (find "Untagged fixnum:" lines
                             :test (lambda (x y) (search x y))))
           (result (when result-line
                    (parse-integer result-line
                                  :start (+ 17 (search "Untagged fixnum:" result-line))
                                  :junk-allowed t))))
      (if result
          (progn
            (format t "  Result: ~D (expected ~D) ~A~%~%"
                    result expected-result
                    (if (= result expected-result) "✓" "✗"))
            (= result expected-result))
          (progn
            (format t "  Error: Failed to execute or parse result~%")
            (format t "  Output: ~S~%~%" output)
            nil)))))

;; Test suite
(defun run-tests ()
  (format t "=== Function Definition Tests ===~%~%")

  (let ((passed 0)
        (total 0))

    ;; Test 1: Simple function definition and call
    (when (test-defun-expr
           '((defun identity (x) x)
             (identity 42))
           42
           "Test 1: Identity function")
      (incf passed))
    (incf total)

    ;; Test 2: Function with arithmetic
    (when (test-defun-expr
           '((defun add (x y) (+ x y))
             (add 10 20))
           30
           "Test 2: Add function")
      (incf passed))
    (incf total)

    ;; Test 3: Function with multiple operations
    (when (test-defun-expr
           '((defun compute (x y z) (* (+ x y) z))
             (compute 3 4 5))
           35
           "Test 3: Multi-operation function")
      (incf passed))
    (incf total)

    ;; Test 4: Function calling another function
    (when (test-defun-expr
           '((defun double (x) (* x 2))
             (defun quad (x) (double (double x)))
             (quad 5))
           20
           "Test 4: Function calling another function")
      (incf passed))
    (incf total)

    ;; Test 5: Recursive function (factorial)
    (when (test-defun-expr
           '((defun fact (n)
               (if (<= n 1)
                   1
                   (* n (fact (- n 1)))))
             (fact 5))
           120
           "Test 5: Recursive factorial")
      (incf passed))
    (incf total)

    ;; Test 6: Function with let binding
    (when (test-defun-expr
           '((defun compute-with-let (x y)
               (let ((sum (+ x y))
                     (diff (- x y)))
                 (* sum diff)))
             (compute-with-let 10 3))
           91  ; (10+3) * (10-3) = 13 * 7 = 91
           "Test 6: Function with let binding")
      (incf passed))
    (incf total)

    ;; Test 7: Nested multiplication uses separate temp slots
    (when (test-defun-expr
           '((defun nested-mul ()
               (* 2 (* 3 4)))
             (nested-mul))
           24
           "Test 7: Nested multiplication")
      (incf passed))
    (incf total)

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
