;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;; host-sbcl.lisp - SBCL Implementation of Host Compatibility Layer

(in-package :host)

;;; === String Operations ===

(defun h-string= (s1 s2)
  (string= s1 s2))

(defun h-string-length (s)
  (length s))

(defun h-string-ref (s i)
  (char-code (schar s i)))

(defun h-string-upcase (s)
  (string-upcase s))

(defun h-substring (s start end)
  (subseq s start end))

(defun h-string-concat (s1 s2)
  (concatenate 'string s1 s2))

;;; === Symbol Operations ===

(defun h-intern (name)
  (intern name))

(defun h-symbol-name (sym)
  (symbol-name sym))

(defun h-make-symbol (name)
  (make-symbol name))

(let ((counter 0))
  (defun h-gensym (&optional (prefix "G"))
    (incf counter)
    (make-symbol (format nil "~A~D" prefix counter))))

;;; === I/O ===

(defun h-file-read-all (path)
  (with-open-file (s path :direction :input)
    (let* ((len (file-length s))
           (str (make-string len)))
      (read-sequence str s)
      str)))

(defun h-file-write-bytes (path bytes)
  (with-open-file (s path :direction :output
                     :if-exists :supersede
                     :element-type '(unsigned-byte 8))
    (dolist (b bytes)
      (write-byte b s))))

(defun h-print-string (str)
  (princ str)
  (finish-output))

(defun h-print-int (n)
  (princ n)
  (finish-output))

(defun h-print-newline ()
  (terpri)
  (finish-output))

;;; === Error Handling ===

(defun h-error (msg)
  (error "~A" msg))

(defun h-fatal (code)
  #+sbcl (sb-ext:exit :code code)
  #-sbcl (error "Fatal: ~A" code))

;;; === Alist Utilities ===

(defun h-acons (key val alist)
  (acons key val alist))

(defun h-alist-get (key alist)
  (let ((pair (assoc key alist :test #'equal)))
    (if pair (cdr pair) nil)))

(defun h-alist-set (key val alist)
  (h-acons key val (h-alist-remove key alist)))

(defun h-alist-remove (key alist)
  (remove key alist :key #'car :test #'equal))

;;; === Feature Detection ===

(defun h-host-name ()
  :sbcl)
