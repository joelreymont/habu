;;;; ASDF system definition for Habu bootstrap compiler

(defsystem "habu"
  :description "Habu Common Lisp bootstrap compiler"
  :version "0.1.0"
  :author "Joel Reymont"
  :license "TBD"
  :depends-on ()
  :components ((:file "compiler")
               (:file "reader" :depends-on ("compiler"))
               (:file "elf-writer" :depends-on ("compiler"))))
