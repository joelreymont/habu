;;; Test: recursive labels in argument, NO heap allocation

(labels ((f (x n)
           (if (= n 0)
               x
               (f (labels ((sum (i acc)
                             (if (= i 0)
                                 acc
                                 (sum (- i 1) (+ acc i)))))
                    (sum 3 0))  ; should return 6
                  (- n 1)))))
  (sys-exit (if (= (f 0 1) 6) 42 1)))
