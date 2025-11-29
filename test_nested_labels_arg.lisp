;;; Test passing nested labels result as function argument

(labels ((outer (n)
           (if (= n 0)
               42
               (outer (let* ((x n))
                        (labels ((inner () x))
                          (- (inner) 1)))))))
  (sys-exit (outer 3)))
