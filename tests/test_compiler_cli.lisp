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

;; Test 6: Labels with tail recursion
(test-compile "labels-fact"
  "(labels ((fact (n acc)
              (if (= n 0) acc (fact (- n 1) (* n acc)))))
     (fact 5 1))"
  120)

;; Test 7: Mutual recursion with labels
(test-compile "mutual-rec"
  "(labels ((even? (n)
              (if (= n 0) 1 (odd? (- n 1))))
            (odd? (n)
              (if (= n 0) 0 (even? (- n 1)))))
     (even? 10))"
  1)

;; Test 8: Dotimes loop
(test-compile "dotimes-sum"
  "(let ((sum 0))
     (dotimes (i 5 sum)
       (setf sum (+ sum i))))"
  10)

;; Test 9: Nested function calls
(test-compile "nested-calls"
  "(defun add (x y) (+ x y))
   (defun mul (x y) (* x y))
   (add (mul 3 4) (mul 2 3))"
  18)

;; Test 10: Higher-order functions with funcall
(test-compile "funcall"
  "(defun apply-twice (f x)
     (funcall f (funcall f x)))
   (defun add5 (n) (+ n 5))
   (apply-twice #'add5 10)"
  20)

;; Test 11: Cons cells and list operations
(test-compile "cons-list"
  "(let ((pair (cons 10 32)))
     (+ (car pair) (cdr pair)))"
  42)

;; Test 12: Conditionals with cond
(test-compile "cond"
  "(defun classify (n)
     (cond ((< n 0) 1)
           ((= n 0) 2)
           (t 3)))
   (+ (classify -1) (classify 0) (classify 1))"
  6)

;; Test 13: Let* with sequential bindings
(test-compile "let-star"
  "(let* ((a 5) (b (* a 2)) (c (+ a b)))
     c)"
  15)

;; Test 14: Complex recursion with tree structure
;; Tree: (cons (cons 1 2) (cons 3 4)) has 3 cons nodes (interior nodes)
;; The atoms 1, 2, 3, 4 are not cons cells and return 0
(test-compile "tree-count"
  "(defun count-nodes (tree)
     (if (consp tree)
         (+ 1 (count-nodes (car tree)) (count-nodes (cdr tree)))
         0))
   (count-nodes (cons (cons 1 2) (cons 3 4)))"
  3)

(format t "~%Results: ~A passed, ~A failed~%~%" *tests-passed* *tests-failed*)
(format t "~%Done.~%")
