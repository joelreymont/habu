;; Test labels with mutual recursion
(defun test-mutual-rec ()
  (labels ((even? (n)
             (if (= n 0) 1 (odd? (- n 1))))
           (odd? (n)
             (if (= n 0) 0 (even? (- n 1)))))
    (even? 10)))

(test-mutual-rec)
