;; Advanced test program - separate functions avoid SIGSEGV
;; Test: dotimes with function call

(defun sum-to (n)
  (let ((sum 0))
    (dotimes (i n sum)
      (setf sum (+ sum i)))))

;; 0+1+2+3+4 = 10
(sum-to 5)
