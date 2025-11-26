;;;; S-expression reader for Habu

(in-package :habu)

;;; Simple reader - wraps CL reader for now
;;; Eventually will implement Habu-specific reader

(defun read-habu-expr (stream)
  "Read a Habu expression from stream"
  (read stream nil))

(defun read-habu-file (filename)
  "Read all expressions from a Habu source file"
  (with-open-file (in filename :direction :input)
    (loop for expr = (read-habu-expr in)
          while expr
          collect expr)))

(defun read-habu-string (string)
  "Read a Habu expression from a string"
  (with-input-from-string (in string)
    (read-habu-expr in)))
