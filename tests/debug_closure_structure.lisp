#!/usr/bin/env sbcl --script
;;; Debug closure structure

(load "run-habu.lisp")

;; Test 1: Simple closure returned by defun
(format t "~%Test 1: Closure returned by make-fn~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((defun make-fn () (lambda (y) (+ #x5 y)))
                 (make-fn)))))
  (format t "Result: ~A (tag=~A)~%" result (logand result #xF)))

;; Test 2: Store in let, then funcall - this works
(format t "~%Test 2: Store in let, then funcall~%")
(format t "Result: ~A~%"
        (habu-sbcl:compile-and-run-forms
         '((defun make-fn () (lambda (y) (+ #x5 y)))
           (let ((f (make-fn)))
             (funcall f #x3)))))

;; Test 3: Direct funcall - this fails
(format t "~%Test 3: Direct funcall~%")
(format t "Result: ~A~%"
        (habu-sbcl:compile-and-run-forms
         '((defun make-fn () (lambda (y) (+ #x5 y)))
           (funcall (make-fn) #x3))))

;; Test 4: Check if the issue is with fn-code being call-fn specifically
(format t "~%Test 4: Funcall on (let ((x closure)) x) - indirection~%")
(format t "Result: ~A~%"
        (habu-sbcl:compile-and-run-forms
         '((defun make-fn () (lambda (y) (+ #x5 y)))
           (funcall (let ((x (make-fn))) x) #x3))))

;; Test 5: Two nested lets
(format t "~%Test 5: Two nested lets~%")
(format t "Result: ~A~%"
        (habu-sbcl:compile-and-run-forms
         '((defun make-fn () (lambda (y) (+ #x5 y)))
           (let ((f (make-fn)))
             (let ((g f))
               (funcall g #x3))))))

(sb-ext:quit :unix-status 0)
