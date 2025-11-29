;;; Test: simple non-recursive labels in argument

(labels ((f (x n)
           (if (= n 0)
               x
               (f (labels ((g (i) (+ i 1)))
                    (g 41))
                  (- n 1)))))
  (sys-exit (f 0 1)))
