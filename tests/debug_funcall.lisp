#!/usr/bin/env sbcl --script
;;; Debug the funcall issue

(load "run-habu.lisp")

(format t "~%=== Working case: let + funcall ===~%")
(let ((working (quote ((defun make-fn () (lambda (y) (+ #x5 y)))
                        (let ((f (make-fn)))
                          (funcall f #x3))))))
  (format t "Result: ~A~%" (habu-sbcl:compile-and-run-forms working)))

(format t "~%=== Failing case: direct funcall ===~%")
(let ((failing (quote ((defun make-fn () (lambda (y) (+ #x5 y)))
                        (funcall (make-fn) #x3)))))
  (format t "Result: ~A~%" (habu-sbcl:compile-and-run-forms failing)))

;; Simpler test without defun
(format t "~%=== Simpler working: let + funcall with inline lambda ===~%")
(format t "Result: ~A~%"
        (habu-sbcl:compile-and-run-forms
         '((let ((f (let ((x #x5)) (lambda (y) (+ x y)))))
             (funcall f #x3)))))

(format t "~%=== Even simpler: direct funcall on lambda-ref ===~%")
(format t "Result: ~A~%"
        (habu-sbcl:compile-and-run-forms
         '((funcall (lambda (y) (+ #x5 y)) #x3))))

(sb-ext:quit :unix-status 0)
