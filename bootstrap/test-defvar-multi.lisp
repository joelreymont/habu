(load "compiler.lisp")
(in-package :habu-compiler)

(initialize-runtime-integration)

(format t "~%Defining variables:~%")
(compile-expression '(defvar *x* 42) :arch :x86_64)
(compile-expression '(defvar *y* 100) :arch :x86_64)
(compile-expression '(defvar *z* 7) :arch :x86_64)  ; NOTE: Can't use 0 - conflicts with unbound marker

(format t "~%Reading *x*:~%")
(compile-expression '(symbol-value '*x*) :arch :x86_64)

(format t "~%Reading *y*:~%")
(compile-expression '(symbol-value '*y*) :arch :x86_64)

(format t "~%Reading *z*:~%")
(compile-expression '(symbol-value '*z*) :arch :x86_64)

(format t "~%Done!~%")
(sb-ext:quit)
