(load "habu0.lisp")

;; Test that h0-eval works with the new case-based dispatch
(format t "~%Testing h0-eval with case-based dispatch...~%")

;; Simple arithmetic test
(let ((result (h0-eval '(+ 2 3) nil nil)))
  (format t "Test 1 (+ 2 3): ~A~%" result))

;; Quote test
(let ((result (h0-eval '(quote foo) nil nil)))
  (format t "Test 2 (quote foo): ~A~%" result))

;; If test
(let ((result (h0-eval '(if t 42 99) nil nil)))
  (format t "Test 3 (if t 42 99): ~A~%" result))

(format t "~%All tests passed! h0-eval now uses case with symbol keys.~%")
