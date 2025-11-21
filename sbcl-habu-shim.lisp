;;;; SBCL shim for loading Habu code (predicates + minimal runtime stubs)

(defpackage :habu-shim
  (:use :cl)
  (:shadow nil? cons? symbol? fixnum? symbol=? list-length
           get-tag string-ref string-length-raw make-vector vector-set
           fill-vec make-string-from-vector make-string-from-cstr
           readline print-value read-file))

(in-package :habu-shim)

(defun nil? (x) (null x))
(defun cons? (x) (consp x))
(defun symbol? (x) (symbolp x))
(defun fixnum? (x) (typep x 'fixnum))
(defun symbol=? (a b) (eq a b))

(defun get-tag (x) 0)  ; stub: everything tagged as 0 for load-time purposes

(defun string-ref (s i) (char-code (aref s i)))
(defun string-length-raw (s) (length s))

(defun make-vector (len) (make-array len))
(defun vector-set (v i val) (setf (aref v i) val) v)
(defun fill-vec (chars vec idx)
  (if (nil? chars) vec
      (progn
        (vector-set vec idx (car chars))
        (fill-vec (cdr chars) vec (+ idx 1)))))
(defun make-string-from-vector (vec)
  (coerce (map 'list #'code-char vec) 'string))
(defun make-string-from-cstr (s) s)

(defun readline (&optional (prompt ""))
  (when prompt (princ prompt) (force-output))
  (read-line *standard-input* nil nil))

(defun print-value (v) (prin1 v))

(defun read-file (path)
  (with-open-file (in path :direction :input)
    (let ((content (make-string (file-length in))))
      (read-sequence content in)
      content)))
