;;; Test nested labels in argument expression

(labels ((outer (n)
           (if (= n 0)
               42
               (outer (- n (labels ((inner (x) x))
                             (inner 1)))))))
  (sys-exit (outer 1)))
