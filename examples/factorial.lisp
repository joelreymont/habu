;; Factorial program - returns factorial of 5 (= 120)
(defun fact (n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(fact 5)
