#!/usr/bin/env sbcl --script
;;; test-let.lisp - Test let bindings

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

(defun test-let-expr (expr expected-result description)
  "Test a let expression"
  (format t "~A~%" description)
  (format t "  Expression: ~S~%" expr)

  (let* ((ir (habu-sbcl-codegen:compile-expr expr nil nil))
         (code (habu-sbcl-codegen:compile-to-arm64 expr))
         (filename "/tmp/test-let.bin"))
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
  (format t "=== Let Binding Tests ===~%~%")

  (let ((passed 0)
        (total 0))

    ;; Simple let binding
    (when (test-let-expr '(let ((x 10)) x) 10
                         "Test 1: Simple let binding")
      (incf passed))
    (incf total)

    ;; Let with computation
    (when (test-let-expr '(let ((x 10)) (+ x 5)) 15
                         "Test 2: Let with arithmetic")
      (incf passed))
    (incf total)

    ;; Multiple bindings
    (when (test-let-expr '(let ((x 10) (y 20)) (+ x y)) 30
                         "Test 3: Multiple let bindings")
      (incf passed))
    (incf total)

    ;; Nested let
    (when (test-let-expr '(let ((x 10))
                            (let ((y 20))
                              (+ x y))) 30
                         "Test 4: Nested let bindings")
      (incf passed))
    (incf total)

    ;; Let with shadowing
    (when (test-let-expr '(let ((x 10))
                            (let ((x 20))
                              x)) 20
                         "Test 5: Let with variable shadowing")
      (incf passed))
    (incf total)

    ;; Let with complex expression
    (when (test-let-expr '(let ((x 5) (y 7))
                            (let ((z (+ x y)))
                              (* z 2))) 24
                         "Test 6: Let with complex expressions")
      (incf passed))
    (incf total)

    ;; Let in conditional
    (when (test-let-expr '(let ((x 10))
                            (if (< x 20)
                                (+ x 5)
                                (- x 5))) 15
                         "Test 7: Let in conditional")
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