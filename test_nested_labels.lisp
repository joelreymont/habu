;;; Test nested labels without string operations

(labels ((outer (n)
           (labels ((inner (m)
                      (if (= m 0)
                          0
                          (+ 1 (inner (- m 1))))))
             (if (= n 0)
                 0
                 (+ (inner 5) (outer (- n 1)))))))
  (let ((result (outer 3)))
    (if (= result 15)
        (sys-exit 42)
        (sys-exit 1))))
