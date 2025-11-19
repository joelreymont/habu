;;;; Test defun and funcall integration

(load "compiler.lisp")
(in-package :habu-compiler)

(initialize-runtime-integration)

(format t "~%Testing defun and funcall~%")
(format t "=========================~%~%")

;; Test 1: Define functions
(format t "1. Defining functions~%")
(compile-expression '(defun add (x y) (+ x y)) :arch :x86_64)
(compile-expression '(defun square (x) (* x x)) :arch :x86_64)
(compile-expression '(defun sum-squares (a b) (+ (square a) (square b))) :arch :x86_64)

;; Test 2: Direct function calls (existing inline behavior)
(format t "~%2. Direct function calls (inline)~%")
(let ((code1 (compile-expression '(add 3 4) :arch :x86_64))
      (code2 (compile-expression '(square 5) :arch :x86_64))
      (code3 (compile-expression '(sum-squares 3 4) :arch :x86_64)))
  (format t "  (add 3 4) => ~D bytes~%" (length code1))
  (format t "  (square 5) => ~D bytes~%" (length code2))
  (format t "  (sum-squares 3 4) => ~D bytes~%" (length code3)))

;; Test 3: funcall - NEW!
(format t "~%3. Function calls via funcall~%")
(let ((code1 (compile-expression '(funcall 'add 10 20) :arch :x86_64))
      (code2 (compile-expression '(funcall 'square 7) :arch :x86_64))
      (code3 (compile-expression '(funcall 'sum-squares 5 12) :arch :x86_64)))
  (format t "  (funcall 'add 10 20) => ~D bytes~%" (length code1))
  (format t "  (funcall 'square 7) => ~D bytes~%" (length code2))
  (format t "  (funcall 'sum-squares 5 12) => ~D bytes~%" (length code3)))

;; Test 4: Verify symbols are interned
(format t "~%4. Verifying symbol table~%")
(let ((sym-table (symbol-value (find-symbol "*SYMBOL-TABLE*" :habu-runtime))))
  (dolist (name '("ADD" "SQUARE" "SUM-SQUARES"))
    (let ((sym (gethash name sym-table)))
      (if sym
          (format t "  ✓ ~A interned at ~X~%" name sym)
          (format t "  ✗ ~A not found~%" name)))))

;; Test 5: Error handling - undefined function
(format t "~%5. Testing error handling~%")
(handler-case
    (progn
      (compile-expression '(funcall 'undefined-function 1 2) :arch :x86_64)
      (format t "  ✗ Should have errored on undefined function~%"))
  (error (e)
    (format t "  ✓ Correctly caught error: ~A~%" e)))

(format t "~%✓ All tests complete!~%")
(sb-ext:quit)
