;; Test habu-compile command-line tool
(require :asdf)
(push (truename "bootstrap/") asdf:*central-registry*)
(asdf:load-system :habu)
(load "macho-linker.lisp")

(format t "~%=== Test compiler CLI ===~%~%")

(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

(defun test-compile (name source-content expected)
  "Compile source to executable and verify exit code"
  (let ((source-path (format nil "/tmp/cli_~A.lisp" name))
        (exec-path (format nil "/tmp/cli_~A" name)))
    (handler-case
        (progn
          ;; Write source file
          (with-open-file (f source-path :direction :output :if-exists :supersede)
            (write-string source-content f))
          ;; Compile
          (habu:deliver-file-with-libsystem source-path exec-path)
          ;; Sign
          (sb-ext:run-program "/usr/bin/codesign" (list "-s" "-" exec-path)
                              :output nil :error nil :wait t)
          ;; Run and check
          (let* ((proc (sb-ext:run-program exec-path nil :output nil :error nil :wait t))
                 (code (sb-ext:process-exit-code proc)))
            (if (= code expected)
                (progn
                  (format t "[PASS] ~A = ~A~%" name code)
                  (incf *tests-passed*))
                (progn
                  (format t "[FAIL] ~A: expected ~A, got ~A~%" name expected code)
                  (incf *tests-failed*)))))
      (error (e)
        (format t "[ERR]  ~A: ~A~%" name e)
        (incf *tests-failed*)))))

;; Test 1: Simple expression
(test-compile "simple" "(+ 20 22)" 42)

;; Test 2: Let binding
(test-compile "let" "(let ((x 5)) (* x 8))" 40)

;; Test 3: Factorial function
(test-compile "factorial"
  "(defun fact (n)
     (if (= n 0)
         1
         (* n (fact (- n 1)))))
   (fact 5)"
  120)

;; Test 4: Fibonacci
(test-compile "fib"
  "(defun fib (n)
     (if (< n 2)
         n
         (+ (fib (- n 1)) (fib (- n 2)))))
   (fib 10)"
  55)

;; Test 5: Multiple functions
(test-compile "multi-fn"
  "(defun double (x) (* x 2))
   (defun triple (x) (* x 3))
   (+ (double 10) (triple 10))"
  50)

(format t "~%Results: ~A passed, ~A failed~%~%" *tests-passed* *tests-failed*)
(format t "~%Done.~%")
