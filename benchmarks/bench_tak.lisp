;;; Takeuchi function benchmark - tests deep recursion and conditionals
(defun tak (x y z)
  (if (>= y x)
      z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y))))

;; tak(18, 12, 6) is a standard benchmark
(tak 18 12 6)
