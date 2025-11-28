;;; Build habu0 native executable
(require :asdf)
(push (truename "bootstrap/") asdf:*central-registry*)
(asdf:load-system :habu)
(in-package :habu)
(load "macho-linker.lisp")
(deliver-file-with-libsystem "habu0.lisp" "habu0" :verbose t)
(format t "~%Built habu0 native executable~%")
(sb-ext:quit :unix-status 0)
