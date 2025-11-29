;;; Test nested labels with vector operations like string-append

(labels ((outer (n)
           (if (= n 0)
               42
               (outer (let ((vec (make-vector 2)))
                        (labels ((copy1 (i)
                                   (if (< i 1)
                                       (progn
                                         (vector-set vec i 88)
                                         (copy1 (+ i 1)))))
                                 (copy2 (i)
                                   (if (< i 1)
                                       (progn
                                         (vector-set vec (+ 1 i) 89)
                                         (copy2 (+ i 1))))))
                          (copy1 0)
                          (copy2 0)
                          (vector-ref vec 0)))))))
  (sys-exit (outer 1)))
