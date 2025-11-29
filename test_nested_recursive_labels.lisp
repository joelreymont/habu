;;; Test nested recursive labels in argument

(labels ((outer (n)
           (if (= n 0)
               42
               (outer (labels ((inner (i)
                                 (if (= i 0)
                                     1
                                     (inner (- i 1)))))
                        (inner 1))))))
  (sys-exit (outer 1)))
