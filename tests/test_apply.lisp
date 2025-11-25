;; Test apply function
(load "run-habu.lisp")

;; Test apply #'append with empty list
(let ((result (habu-sbcl:compile-and-run-forms
               '((apply (function append) nil)))))
  (format t "apply append nil: ~A (expected 0 = nil)~%" result))

;; Test apply #'append with single list
(let ((result (habu-sbcl:compile-and-run-forms
               '((car (apply (function append) (list (list #x42))))))))
  (format t "apply append single: ~A (expected 66)~%" result))

;; Test apply #'append with two lists
(let ((result (habu-sbcl:compile-and-run-forms
               '((let ((lists (list (list #x1 #x2) (list #x3 #x4))))
                   (car (apply (function append) lists)))))))
  (format t "apply append two lists first: ~A (expected 1)~%" result))

(let ((result (habu-sbcl:compile-and-run-forms
               '((let ((lists (list (list #x1 #x2) (list #x3 #x4))))
                   (caddr (apply (function append) lists)))))))
  (format t "apply append two lists third: ~A (expected 3)~%" result))

;; Test apply #'max with single element
(let ((result (habu-sbcl:compile-and-run-forms
               '((apply (function max) (list #x42))))))
  (format t "apply max single: ~A (expected 66)~%" result))

;; Test apply #'max with multiple elements
(let ((result (habu-sbcl:compile-and-run-forms
               '((apply (function max) (list #x1 #x5 #x3 #x2))))))
  (format t "apply max multiple: ~A (expected 5)~%" result))

;; Test general apply with 0 args
(let ((result (habu-sbcl:compile-and-run-forms
               '((apply (lambda () #x42) nil)))))
  (format t "apply lambda 0 args: ~A (expected 66)~%" result))

;; Test general apply with 1 arg
(let ((result (habu-sbcl:compile-and-run-forms
               '((apply (lambda (x) (+ x #x1)) (list #x9))))))
  (format t "apply lambda 1 arg: ~A (expected 10)~%" result))

;; Test general apply with 2 args
(let ((result (habu-sbcl:compile-and-run-forms
               '((apply (lambda (a b) (+ a b)) (list #x3 #x5))))))
  (format t "apply lambda 2 args: ~A (expected 8)~%" result))

;; Test general apply with 3 args
(let ((result (habu-sbcl:compile-and-run-forms
               '((apply (lambda (a b c) (+ a (+ b c))) (list #x1 #x2 #x3))))))
  (format t "apply lambda 3 args: ~A (expected 6)~%" result))

(sb-ext:quit :unix-status 0)
