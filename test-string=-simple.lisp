(defun test-nested-if (a b c d)
  (if (>= a b)
      42
      (if (= c d)
          (test-nested-if a b c (+ d 1))
          99)))

(defun main ()
  (test-nested-if 0 5 10 10))
