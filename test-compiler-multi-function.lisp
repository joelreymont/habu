;;;; Test multi-function compilation in SBCL environment
;;;; Tests the compile-program-with-functions-with-runtime pipeline

(load "sbcl-habu-shim.lisp")
(load "habu-arm64-codegen-sbcl.lisp")

(in-package :habu-sbcl-codegen)

(format t "~%=== Multi-Function Compilation Tests ===~%~%")

;;; Test 1: Simple program with one function
(format t "Test 1: Program with one function definition~%")
(let* ((forms '((defun add-one (x) (+ x 1))
                42))
       (code (compile-program-with-functions-with-runtime forms nil)))
  (format t "  Result: ~a bytes of code generated~%" (length code))
  (if (> (length code) 0)
      (format t "  ✓ PASS - Code generated~%~%")
      (format t "  ✗ FAIL - No code generated~%~%")))

;;; Test 2: Program with multiple functions
(format t "Test 2: Program with multiple function definitions~%")
(let* ((forms '((defun add (a b) (+ a b))
                (defun multiply (a b) (* a b))
                (add 3 4)))
       (code (compile-program-with-functions-with-runtime forms nil)))
  (format t "  Result: ~a bytes of code generated~%" (length code))
  (if (> (length code) 0)
      (format t "  ✓ PASS - Code generated~%~%")
      (format t "  ✗ FAIL - No code generated~%~%")))

;;; Test 3: Verify function offset calculation
(format t "Test 3: Function offset calculation~%")
(let* ((forms '((defun foo (x) x)
                (defun bar (y) y)
                123))
       (compile-result (compile-forms forms)))
  (format t "  Compiled functions: ~a~%" (car compile-result))
  (format t "  Main IR: ~a~%" (cadr compile-result))
  (let ((fns-result (codegen-functions-helper (car compile-result) 0 nil)))
    (format t "  Function offsets: ~a~%" (cadr fns-result))
    (if (= (length (cadr fns-result)) 2)
        (format t "  ✓ PASS - Both functions have offsets~%~%")
        (format t "  ✗ FAIL - Expected 2 function offsets~%~%"))))

;;; Test 4: Complete pipeline with runtime addresses
(format t "Test 4: Pipeline with runtime addresses~%")
(let* ((runtime-addrs (make-runtime-addrs #x1000 #x2000 #x3000))
       (forms '((defun identity (x) x)
                42))
       (code (compile-program-with-functions-with-runtime forms runtime-addrs)))
  (format t "  Runtime addresses: ~a~%" runtime-addrs)
  (format t "  Code length: ~a bytes~%" (length code))
  (if (> (length code) 0)
      (format t "  ✓ PASS - Pipeline works with runtime addresses~%~%")
      (format t "  ✗ FAIL - Pipeline failed~%~%")))

;;; Test 5: Count instructions helper
(format t "Test 5: Instruction counting~%")
(let* ((code '(1 2 3 4   ; 1 instruction
               5 6 7 8   ; 2 instructions
               9 10 11 12)) ; 3 instructions
       (count (count-instrs code)))
  (format t "  Code: 12 bytes~%")
  (format t "  Count: ~a instructions~%" count)
  (if (= count 3)
      (format t "  ✓ PASS - Correctly counted 3 instructions~%~%")
      (format t "  ✗ FAIL - Expected 3, got ~a~%~%" count)))

(format t "=== All Tests Complete ===~%~%")
