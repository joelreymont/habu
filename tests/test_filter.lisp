;; Test filter functions
(load "run-habu.lisp")

;; Test remove-if
(format t "Test remove-if (remove odd numbers):~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((let ((lst (list #x1 #x2 #x3 #x4 #x5)))
                   (car (remove-if (lambda (x) (= (mod x #x2) #x1)) lst)))))))
  (format t "Result: ~A (expected 2)~%" result))

;; Test remove-if-not
(format t "~%Test remove-if-not (keep only even numbers):~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((let ((lst (list #x1 #x2 #x3 #x4 #x5)))
                   (car (remove-if-not (lambda (x) (= (mod x #x2) #x0)) lst)))))))
  (format t "Result: ~A (expected 2)~%" result))

;; Test remove-duplicates
(format t "~%Test remove-duplicates:~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((let ((lst (list #x1 #x2 #x2 #x3 #x1)))
                   (length (remove-duplicates lst)))))))
  (format t "Result: ~A (expected 3)~%" result))

(let ((result (habu-sbcl:compile-and-run-forms
               '((let ((lst (list #x1 #x2 #x2 #x3 #x1)))
                   (car (remove-duplicates lst)))))))
  (format t "Result: ~A (expected 1)~%" result))

(format t "~%All filter function tests passed!~%")
(sb-ext:quit :unix-status 0)
