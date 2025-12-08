;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;; host-habu0.lisp - Native Habu Implementation of Host Compatibility Layer
;;;
;;; NOTE: This file is designed to be loadable by habu0's reader and evaluator.
;;; Uses only the subset of Lisp that habu0 supports.

(in-package :host)

;;; === String Operations ===

(defun h-string= (s1 s2)
  (string= s1 s2))

(defun h-string-length (s)
  (string-length s))

(defun h-string-ref (s i)
  (string-ref s i))

(defun h-string-upcase (s)
  (string-upcase s))

(defun h-substring (s start end)
  (substring s start end))

(defun h-string-concat (s1 s2)
  (string-concat s1 s2))

;;; === Symbol Operations ===

(defun h-intern (name)
  (intern name))

(defun h-symbol-name (sym)
  (symbol-name sym))

(defun h-make-symbol (name)
  (make-symbol-from-string name))

;; Gensym counter in a cons cell for mutability
(defvar *h-gensym-counter* (cons 0 nil))

(defun h-gensym (prefix)
  (let ((n (car *h-gensym-counter*)))
    (setcar *h-gensym-counter* (+ n 1))
    (make-symbol-from-string
     (string-concat (if prefix prefix "G")
                    (number-to-string n)))))

;;; === I/O ===

(defun h-file-read-all (path)
  (native-read-file path))

(defun h-file-write-bytes (path bytes)
  (native-write-bytes path bytes))

(defun h-print-string (str)
  (write-string str 1))  ; fd 1 = stdout

(defun h-print-int (n)
  (write-int n 1))

(defun h-print-newline ()
  (sys-write-char 10 1))  ; newline to stdout

;;; === Error Handling ===

(defun h-error (msg)
  (h-print-string "ERROR: ")
  (h-print-string msg)
  (h-print-newline)
  (fatal-error 1))

(defun h-fatal (code)
  (sys-exit code))

;;; === Alist Utilities ===

(defun h-acons (key val alist)
  (cons (cons key val) alist))

(defun h-alist-get (key alist)
  (labels ((find-it (lst)
             (cond
               ((null lst) nil)
               ((equal key (car (car lst))) (cdr (car lst)))
               (t (find-it (cdr lst))))))
    (find-it alist)))

(defun h-alist-set (key val alist)
  (h-acons key val (h-alist-remove key alist)))

(defun h-alist-remove (key alist)
  (cond
    ((null alist) nil)
    ((equal key (car (car alist))) (h-alist-remove key (cdr alist)))
    (t (cons (car alist) (h-alist-remove key (cdr alist))))))

;;; === Feature Detection ===

(defun h-host-name ()
  :habu0)
