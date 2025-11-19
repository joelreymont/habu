;;;; Test quasiquote/backquote in macros

(load "compiler.lisp")
(in-package :habu-compiler)

(initialize-runtime-integration)

(format t "~%Testing Macros with Quasiquote~%")
(format t "================================~%~%")

;; Test 1: Simple macro without quasiquote (current working approach)
(format t "1. Simple macro (no quasiquote)~%")
(compile-expression '(defmacro double (x) (+ x x)) :arch :x86_64)
(let ((code (compile-expression '(double 5) :arch :x86_64)))
  (format t "  (double 5) compiles to ~D bytes~%" (length code)))

;; Test 2: Try macro with quasiquote (backquote)
(format t "~%2. Macro with quasiquote~%")
(handler-case
    (progn
      (compile-expression '(defmacro my-when (test body)
                             `(if ,test ,body 0))
                          :arch :x86_64)
      (let ((code (compile-expression '(my-when (> 5 3) 42) :arch :x86_64)))
        (format t "  ✓ (my-when (> 5 3) 42) compiles to ~D bytes~%" (length code))))
  (error (e)
    (format t "  ✗ Error: ~A~%" e)))

;; Test 3: Try macro with splicing
(format t "~%3. Macro with unquote-splicing~%")
(handler-case
    (progn
      (compile-expression '(defmacro my-progn (body rest)
                             `(progn ,body ,@rest))
                          :arch :x86_64)
      (let ((code (compile-expression '(my-progn 1 (2 3 4)) :arch :x86_64)))
        (format t "  ✓ (my-progn 1 (2 3 4)) compiles to ~D bytes~%" (length code))))
  (error (e)
    (format t "  ✗ Error: ~A~%" e)))

;; Test 4: Standard when macro (useful test case)
(format t "~%4. Standard when macro~%")
(handler-case
    (progn
      (compile-expression '(defmacro when (test body)
                             `(if ,test ,body 0))
                          :arch :x86_64)
      (let ((code (compile-expression '(when (> 5 3) 42) :arch :x86_64)))
        (format t "  ✓ (when (> 5 3) 42) compiles to ~D bytes~%" (length code))))
  (error (e)
    (format t "  ✗ Error: ~A~%" e)))

(format t "~%Done!~%")
(sb-ext:quit)
