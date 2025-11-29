;;; Test: labels funcall, then nested labels (labels inside labels)

(labels ((outer (n)
           (if (= n 0)
               10
               (outer (- n 1)))))
  (let ((result1 (outer 1)))
    ;; Now nested labels like concat has
    (labels ((inner-outer (m)
               (if (= m 0)
                   20
                   (labels ((inner-inner (k)
                              (if (= k 0)
                                  30
                                  (inner-inner (- k 1)))))
                     (+ (inner-inner 1) (inner-outer (- m 1)))))))
      (let ((result2 (inner-outer 1)))
        (sys-write 1 (number-to-string (+ result1 result2)) 2)
        (sys-write 1 "\n" 1)
        (sys-exit 42)))))
