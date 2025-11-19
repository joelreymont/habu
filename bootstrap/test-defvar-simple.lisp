(load "compiler.lisp")
(in-package :habu-compiler)

(initialize-runtime-integration)

(format t "~%Test: (defvar *x* 42)~%")
(compile-expression '(defvar *x* 42) :arch :x86_64)

(format t "~%Test: (symbol-value '*x*)~%")
(compile-expression '(symbol-value '*x*) :arch :x86_64)

(format t "~%Done!~%")
(sb-ext:quit)
