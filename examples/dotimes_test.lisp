;; Test dotimes iteration
(defun sum-to (n)
  (let ((sum 0))
    (dotimes (i n sum)
      (setf sum (+ sum i)))))

(sum-to 5)
