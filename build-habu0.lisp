;;; Build habu0 native executable
(require :asdf)
(push (truename "bootstrap/") asdf:*central-registry*)
(asdf:load-system :habu)
(in-package :habu)
(load "bootstrap/macho.lisp")
(deliver-file "habu0.lisp" "habu0")
(format t "~%Built habu0 native executable~%")
(sb-ext:quit :unix-status 0)
