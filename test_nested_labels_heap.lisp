;;; Test nested labels with heap allocation in argument

(labels ((outer (v n)
           (if (= n 0)
               42
               (outer (labels ((helper (i)
                                 (if (< i 1)
                                     (make-vector 1)
                                     (helper (- i 1)))))
                        (helper 0))
                      (- n 1)))))
  (sys-exit (outer (make-vector 1) 1)))
