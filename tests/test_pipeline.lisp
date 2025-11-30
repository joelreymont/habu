;;; Test pure compiler pipeline - no SBCL dependencies in generated code
(load "bootstrap/compiler.lisp")
(load "bootstrap/macho.lisp")
(load "bootstrap/reader.lisp")
(load "bootstrap/compiler.lisp")
(load "bootstrap/codegen.lisp")
(load "bootstrap/macho-utils.lisp")

(format t "~%=== Testing Pure Compiler Pipeline ===~%")

(defvar *pass-count* 0)
(defvar *fail-count* 0)

(defun test-pure (name source expected)
  (handler-case
      (let ((output-path (format nil "/tmp/test_pure_~A" name)))
        (habu:deliver-v3 source output-path)
        (let* ((proc (sb-ext:run-program output-path nil :output nil :error nil :wait t))
               (result (sb-ext:process-exit-code proc)))
          (if (= result expected)
              (progn (format t "[PASS] ~A = ~A~%" name result) (incf *pass-count*))
              (progn (format t "[FAIL] ~A: expected ~A, got ~A~%" name expected result) (incf *fail-count*)))))
    (error (e)
      (format t "[FAIL] ~A: error ~A~%" name e)
      (incf *fail-count*))))

;; Test 1: Simple arithmetic
(test-pure "add" "(sys-exit (+ 20 22))" 42)

;; Test 2: Multiplication
(test-pure "mul" "(sys-exit (* 6 7))" 42)

;; Test 3: Let binding
(test-pure "let" "(sys-exit (let ((x 40)) (+ x 2)))" 42)

;; Test 4: Defun
(test-pure "defun" "(defun double (x) (* x 2)) (sys-exit (double 21))" 42)

;; Test 5: Factorial
(test-pure "fact" "(defun fact (n acc) (if (= n 0) acc (fact (- n 1) (* n acc)))) (sys-exit (fact 5 1))" 120)

;; Test 6: Nested let
(test-pure "let-nested" "(sys-exit (let ((x 10)) (let ((y 20)) (+ x (+ y 12)))))" 42)

;; Test 7: Comparison
(test-pure "cmp" "(sys-exit (if (= 1 1) 42 0))" 42)

;; Summary
(format t "~%~A/~A tests passed~%" *pass-count* (+ *pass-count* *fail-count*))
(when (> *fail-count* 0)
  (sb-ext:exit :code 1))
