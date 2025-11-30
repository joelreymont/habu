;;; Test read-all
(load "bootstrap/compiler.lisp")
(load "bootstrap/reader.lisp")
(format t "Testing read-all...~%")
(let ((result (habu::read-all "(sys-exit (+ 20 22))")))
  (format t "Result: ~S~%" result))
