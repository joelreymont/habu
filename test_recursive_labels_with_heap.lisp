;;; Test: recursive labels WITH heap allocation in argument

(labels ((f (x n)
           (if (= n 0)
               x
               (f (labels ((alloc (i)
                             (if (= i 0)
                                 42
                                 (progn
                                   (make-vector 1)  ; heap allocation
                                   (alloc (- i 1))))))
                    (alloc 2))
                  (- n 1)))))
  (sys-exit (f 0 1)))
