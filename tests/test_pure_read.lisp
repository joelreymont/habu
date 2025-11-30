;;; Test pure-read-all
(load "bootstrap/compiler.lisp")
(load "bootstrap/reader-pure.lisp")
(format t "Testing pure-read-all...~%")
(let ((result (habu::pure-read-all "(sys-exit (+ 20 22))")))
  (format t "Result: ~S~%" result))
