;; Test countdown recursive function
(defun countdown (n)
  (if (= n 0)
      42
      (countdown (- n 1))))

(countdown 5)
