(load "compiler.lisp")
(in-package :habu-compiler)

(format t "~%Test 1: Define square macro~%")
(compile-expression '(defmacro square (x) (* x x)) :arch :x86_64)

(format t "~%Test 2: Use square macro directly~%")
(let ((code (compile-expression '(square 5) :arch :x86_64)))
  (format t "  (square 5) => ~D bytes~%" (length code)))

(format t "~%Test 3: Define quad macro that uses square~%")
(compile-expression '(defmacro quad (x) (square (square x))) :arch :x86_64)

(format t "~%Test 4: Use quad macro~%")
(handler-case
    (let ((code (compile-expression '(quad 2) :arch :x86_64)))
      (format t "  (quad 2) => ~D bytes~%" (length code)))
  (error (e)
    (format t "  Error: ~A~%" e)))

(sb-ext:quit)
