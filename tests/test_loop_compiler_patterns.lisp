;; Test loop patterns used in the actual compiler
(load "run-habu.lisp")

;; Test: (loop for ch across s collect (char-code ch))
(format t "Test: loop for ch across string collect char-code~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((let ((s "ABC"))
                   (car (loop for ch across s collect ch)))))))
  (format t "Result: ~A (expected 65 = char-code of A)~%" result))

;; Test: (loop for i from 0 below n collect i)
(format t "~%Test: loop for i from 0 below n collect i~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((length (loop for i from #x0 below #x5 collect i))))))
  (format t "Result: ~A (expected 5)~%" result))

;; Test: (loop for el in list for idx from 0 do ...)
(format t "~%Test: loop for el in list for idx from 0 do~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((let ((sum #x0))
                   (loop for el in (list #x1 #x2 #x3) for idx from #x0 do
                     (setq sum (+ sum el)))
                   sum)))))
  (format t "Result: ~A (expected 6)~%" result))

(format t "~%All compiler loop patterns work!~%")
(sb-ext:quit :unix-status 0)
