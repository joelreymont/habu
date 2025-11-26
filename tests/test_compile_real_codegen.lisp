#!/usr/bin/env sbcl --script
;;; Test: Compile the Real Habu Codegen
;;;
;;; Attempt to compile functions from habu-arm64-codegen-clean.lisp
;;; using the Habu compiler.

(load "run-habu.lisp")

(format t "~%=== COMPILING REAL HABU CODEGEN ===~%~%")

;; Read forms from file, letting SBCL handle #-sbcl conditionals
(defun read-forms-from-file (path)
  (with-open-file (in path :direction :input)
    (loop for form = (read in nil :eof)
          until (eq form :eof)
          collect form)))

;; Use arm64/codegen-sbcl.lisp which is SBCL-compatible
;; (habu-arm64-codegen-clean.lisp uses Habu-specific syntax like cons?)
(format t "Reading arm64/codegen-sbcl.lisp...~%")
(defparameter *codegen-forms* (read-forms-from-file "arm64/codegen-sbcl.lisp"))
(format t "Read ~D forms~%" (length *codegen-forms*))

;; Count defuns
(defparameter *defun-forms*
  (remove-if-not (lambda (f) (and (consp f) (eq (car f) 'defun))) *codegen-forms*))
(format t "Found ~D defun forms~%~%" (length *defun-forms*))

;; Try to compile incrementally, adding functions one by one
(defun try-compile-forms (forms)
  "Try to compile FORMS with Habu, return (success . error-or-result)"
  (handler-case
      (let ((result (habu-sbcl:compile-and-run-forms
                     (append forms '(#x1)))))  ; Return 1 if successful
        (cons t result))
    (error (e)
      (cons nil (format nil "~A" e)))))

;; Test: Compile first N functions
(defun test-compile-first-n (n)
  (format t "Testing compilation of first ~D functions...~%" n)
  (let* ((forms (subseq *defun-forms* 0 (min n (length *defun-forms*))))
         (result (try-compile-forms forms)))
    (if (car result)
        (format t "  SUCCESS: Compiled ~D functions~%" n)
        (format t "  FAILED: ~A~%" (cdr result)))
    (car result)))

;; Binary search to find how many functions we can compile
(defun find-max-compilable ()
  (let ((max-working 0))
    (loop for n from 1 to (length *defun-forms*)
          do (if (test-compile-first-n n)
                 (setf max-working n)
                 (return)))
    max-working))

;; Start with small batches
(format t "=== Phase 1: Test compilation in batches ===~%~%")

(test-compile-first-n 5)
(test-compile-first-n 10)
(test-compile-first-n 20)
(test-compile-first-n 30)
(test-compile-first-n 40)
(test-compile-first-n 50)

(format t "~%=== Phase 2: Find maximum compilable ===~%~%")
(let ((max-n (find-max-compilable)))
  (format t "~%Maximum functions that compile: ~D / ~D~%"
          max-n (length *defun-forms*))
  (when (< max-n (length *defun-forms*))
    (format t "~%Problematic function #~D: ~A~%"
            (1+ max-n)
            (car (nth max-n *defun-forms*)))))

(sb-ext:quit :unix-status 0)
