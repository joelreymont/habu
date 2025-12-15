;;;; paren-check.lisp - Check paren balance using CL reader
;;;; Usage: sbcl --script tools/paren-check.lisp file.lisp

(defun check-file (path)
  (handler-case
      (with-open-file (s path)
        (let ((*read-suppress* t)) ; Don't intern symbols, just parse
          (loop for form = (read s nil :eof)
                until (eq form :eof)))
        (format t "File ~A is balanced.~%" path)
        0)
    (end-of-file (e)
      (declare (ignore e))
      (format t "Unexpected EOF - unclosed paren in ~A~%" path)
      1)
    (reader-error (e)
      (format t "Reader error in ~A: ~A~%" path e)
      1)))

(let ((file (second sb-ext:*posix-argv*)))
  (if file
      (sb-ext:exit :code (check-file file))
      (progn
        (format t "Usage: paren-check.lisp <file>~%")
        (sb-ext:exit :code 1))))
