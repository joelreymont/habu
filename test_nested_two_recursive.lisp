;;; Test nested labels with TWO recursive functions

(labels ((outer (n)
           (if (= n 0)
               42
               (outer (labels ((f1 (i) (if (= i 0) 10 (f1 (- i 1)))))
                               (f2 (i) (if (= i 0) 20 (f2 (- i 1))))))
                        (progn
                          (f1 1)
                          (f2 1)
                          1))))))
  (sys-exit (outer 1)))
