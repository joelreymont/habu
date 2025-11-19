;;;; Test runtime funcall - calling functions via symbol lookup

(load "compiler.lisp")
(in-package :habu-compiler)

(initialize-runtime-integration)

(format t "~%Testing Runtime Funcall~%")
(format t "=======================~%~%")

;; Test 1: Simple runtime funcall
(format t "1. Simple runtime funcall~%")
(format t "   (defun add (x y) (+ x y))~%")
(compile-expression '(defun add (x y) (+ x y)) :arch :x86_64)

(format t "   (funcall 'add 3 4) - compile-time works~%")
(let ((code (compile-expression '(funcall 'add 3 4) :arch :x86_64)))
  (format t "   Compiled to ~D bytes~%~%" (length code)))

;; For runtime funcall, we need to actually execute the code
;; This test verifies the architecture is in place

(format t "2. Check symbol-function slot~%")
(let* ((sym-table (symbol-value (find-symbol "*SYMBOL-TABLE*" :habu-runtime)))
       (add-sym (gethash "ADD" sym-table))
       (get-fn (find-symbol "RUNTIME-SYMBOL-FUNCTION" :habu-runtime)))
  (handler-case
      (let ((fn-val (funcall get-fn add-sym)))
        (format t "   Symbol 'ADD function slot: ~X~%" fn-val)
        (if (= fn-val 0)
            (format t "   ⚠ Function slot is unbound (0) - need to store code pointer~%")
            (format t "   ✓ Function slot set~%")))
    (error (e)
      (format t "   Error: ~A~%" e))))

(format t "~%Next steps for runtime funcall:~%")
(format t "1. Store compiled code pointer in symbol-function slot~%")
(format t "2. Generate code to call via function pointer~%")
(format t "3. Handle argument passing and return values~%")

(sb-ext:quit)
