;;; Test: TWO recursive labels with heap allocation in argument

(labels ((f (x n)
           (if (= n 0)
               x
               (f (labels ((f1 (i)
                             (if (= i 0)
                                 10
                                 (progn
                                   (make-vector 1)
                                   (f1 (- i 1)))))
                           (f2 (i)
                             (if (= i 0)
                                 20
                                 (progn
                                   (make-vector 1)
                                   (f2 (- i 1))))))
                    (progn
                      (f1 1)
                      (f2 1)
                      42))
                  (- n 1)))))
  (sys-exit (f 0 1)))
