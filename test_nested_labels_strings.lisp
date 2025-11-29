;;; Nested labels with string operations

(labels ((outer (s n)
           (labels ((inner (i)
                      (if (= i 0)
                          s
                          (let ((next (string-append s "X")))
                            (inner (- i 1))))))
             (if (= n 0)
                 s
                 (let ((result (inner 2)))
                   (outer result (- n 1)))))))
  (let ((result (outer "A" 2)))
    (sys-write 1 result (string-length result))
    (sys-write 1 "\n" 1)
    (sys-exit (if (= (string-length result) 5) 42 1))))
