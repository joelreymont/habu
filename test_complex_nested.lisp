;;; Test complex nested pattern similar to string-append

(labels ((outer (n acc)
           (if (= n 0)
               acc
               (outer (- n 1)
                      (let* ((s1 acc)
                             (s2 1)
                             (len1 (+ s1 0))
                             (len2 (+ s2 0))
                             (total (+ len1 len2))
                             (vec (make-vector 2)))
                        (labels ((copy1 (i)
                                   (if (< i 1)
                                       (progn
                                         (vector-set vec i s1)
                                         (copy1 (+ i 1)))))
                                 (copy2 (i)
                                   (if (< i 1)
                                       (progn
                                         (vector-set vec (+ i 1) s2)
                                         (copy2 (+ i 1))))))
                          (copy1 0)
                          (copy2 0)
                          total))))))
  (sys-exit (if (= (outer 3 40) 42) 42 1)))
