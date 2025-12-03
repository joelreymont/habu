;;;; start-slynk.lisp - Start Slynk server with Habu loaded
;;;;
;;;; Usage: sbcl --load start-slynk.lisp
;;;; Then connect from Sly/Emacs or via the MCP bridge

(require :asdf)

;;; Load Slynk
(ql:quickload :slynk :silent t)

;;; Load Habu compiler
(let ((*default-pathname-defaults*
       (make-pathname :directory (butlast (pathname-directory *load-truename*)))))
  (push *default-pathname-defaults* asdf:*central-registry*)
  (asdf:load-system :habu))

;;; Start Slynk server on port 4005 (default Sly port)
(defvar *slynk-port* 4005)

(format t "~%Starting Slynk server on port ~D...~%" *slynk-port*)
(slynk:create-server :port *slynk-port* :dont-close t)

(format t "~%Habu MCP/Slynk server ready.~%")
(format t "Connect from Emacs: M-x sly-connect RET localhost RET ~D~%" *slynk-port*)
(format t "~%Habu package loaded. Try:~%")
(format t "  (habu:compile-program '((+ 1 2)))~%")
(format t "  (habu:deliver \"(defun add2 (x) (+ x 2)) (add2 40)\" \"/tmp/test\")~%")
(format t "~%")

;;; Keep running
(loop (sleep 3600))
