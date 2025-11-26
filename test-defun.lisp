#!/usr/bin/env sbcl --script
;;; test-defun.lisp - Test function definition and calls

(load "sbcl-habu-shim.lisp")
(load "arm64/codegen-sbcl.lisp")

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

    ;; Test 8: Deep nested addition stresses temp-depth
    (when (test-defun-expr
           '((defun deep-nested ()
               (+ 1 (+ 2 (+ 3 (+ 4 (+ 5 (+ 6 (+ 7 8))))))))
             (deep-nested))
           36
           "Test 8: Deep nested addition")
      (incf passed))
    (incf total)

    ;; Test 9: Very deep nesting stays within temp guard
    (when (test-defun-expr
           '((defun very-deep ()
               (+ 1 (+ 2 (+ 3 (+ 4 (+ 5 (+ 6 (+ 7 (+ 8 (+ 9 (+ 10 (+ 11 (+ 12 (+ 13 (+ 14 (+ 15 (+ 16 (+ 17 18))))))))))))))))))
             (very-deep))
           171
           "Test 9: Very deep nested addition")
      (incf passed))
    (incf total)

    ;; Test 10: Inline lambda call
    (when (test-defun-expr
           '(((lambda (x) (+ x 1)) 5))
           6
           "Test 10: Inline lambda call")
      (incf passed))
    (incf total)

    ;; Test 11: Funcall of returned closure (no capture)
    (when (test-defun-expr
           '((defun make-adder () (lambda (y) (+ y 2)))
             (funcall (make-adder) 3))
           5
           "Test 11: Funcall of returned closure")
      (incf passed))
    (incf total)

    ;; Test 12: Inline lambda captures let binding
    (when (test-defun-expr
           '((let ((x 10))
               (funcall (lambda (y) (+ x y)) 5)))
           15
           "Test 12: Inline lambda capturing let binding")
      (incf passed))
    (incf total)

    ;; Test 13: Returned closure captures parameter
    (when (test-defun-expr
           '((defun make-adder (n) (lambda (y) (+ n y)))
             (funcall (make-adder 7) 8))
           15
           "Test 13: Returned closure capturing parameter")
      (incf passed))
    (incf total)

    ;; Test 14: Nested closure capture chain
    (when (test-defun-expr
           '((defun outer (x)
               (lambda (y)
                 (lambda (z) (+ x (+ y z)))))
             (funcall (funcall (outer 1) 2) 3))
           6
           "Test 14: Nested closure capture chain")
      (incf passed))
    (incf total)

    ;; Test 15: Recursive closure with capture
    (when (test-defun-expr
           '((defun make-counter (n)
               (lambda (k)
                 (if (= k 0)
                     n
                     (funcall (make-counter (+ n 1)) (- k 1)))))
             (funcall (make-counter 5) 3))
           8
           "Test 15: Recursive closure with capture")
      (incf passed))
    (incf total)

    ;; Test 16: Multiple captures in closure
    (when (test-defun-expr
           '((let ((a 2) (b 3) (c 4))
               (funcall (lambda (x) (+ x (+ a (+ b c)))) 1)))
           10
           "Test 16: Multiple captured vars")
      (incf passed))
    (incf total)

    ;; Test 17: Higher-arity captures and args
    (when (test-defun-expr
           '((defun make-sum (a b c)
               (lambda (x y) (+ a (+ b (+ c (+ x y))))))
             (funcall (make-sum 1 2 3) 4 5))
           15
           "Test 17: Higher-arity captures and args")
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
