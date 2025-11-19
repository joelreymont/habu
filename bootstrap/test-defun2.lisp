;;;; Test defun symbol integration

(load "compiler.lisp")
(in-package :habu-compiler)

(initialize-runtime-integration)

(format t "~%Testing defun Symbol Integration~%")
(format t "=================================~%~%")

;; Compile defun - this should intern the symbol
(format t "1. Compiling defun...~%")
(compile-expression '(defun add (x y) (+ x y)) :arch :x86_64)

;; Check if symbol was interned
(format t "~%2. Checking if 'ADD was interned...~%")
(let ((sym-table (symbol-value (find-symbol "*SYMBOL-TABLE*" :habu-runtime))))
  (let ((add-sym (gethash "ADD" sym-table)))
    (if add-sym
        (progn
          (format t "  ✓ Symbol 'ADD interned: ~X~%" add-sym)
          ;; Check function slot
          (let ((get-fn (find-symbol "RUNTIME-SYMBOL-FUNCTION" :habu-runtime)))
            (handler-case
                (let ((fn-val (funcall get-fn add-sym)))
                  (format t "  ✓ Function slot set: ~X~%~%" fn-val))
              (error (e)
                (format t "  Function slot: ~A~%~%" e)))))
        (format t "  ✗ Symbol 'ADD not found~%~%"))))

;; Test function call (inline expansion)
(format t "3. Testing function call (inline)~%")
(let ((code (compile-expression '(add 3 4) :arch :x86_64)))
  (format t "  (add 3 4) compiles to ~D bytes~%~%" (length code)))

(format t "✓ Tests complete!~%")
(sb-ext:quit)
