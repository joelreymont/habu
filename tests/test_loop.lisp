;; Test loop macro
(load "run-habu.lisp")

(defun run-test (name forms expected)
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (format t "~A: pass~%" name)
        (format t "~A: FAIL (got ~A, expected ~A)~%" name result expected))))

;; Test: loop for i from 0 below 5 collect i
(format t "loop-for-from-below-collect:~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((car (loop for i from #x0 below #x5 collect i))))))
  (format t "Result: ~A (expected 0)~%" result))

;; Test: loop for i from 0 below 3 collect i - get second element
(let ((result (habu-sbcl:compile-and-run-forms
               '((cadr (loop for i from #x0 below #x3 collect i))))))
  (format t "Result: ~A (expected 1)~%" result))

;; Test: loop for el in list collect el
(format t "~%loop-for-in-collect:~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((let ((lst (list #x1 #x2 #x3)))
                   (car (loop for el in lst collect el)))))))
  (format t "Result: ~A (expected 1)~%" result))

;; Test: loop for el in list collect (* el 2)
(let ((result (habu-sbcl:compile-and-run-forms
               '((let ((lst (list #x1 #x2 #x3)))
                   (cadr (loop for el in lst collect (* el #x2))))))))
  (format t "Result: ~A (expected 4)~%" result))

(sb-ext:quit :unix-status 0)
