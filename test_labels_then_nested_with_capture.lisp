;;; Test: labels funcall, then nested labels that CAPTURE outer variable

(labels ((outer (n)
           (if (= n 0)
               10
               (outer (- n 1)))))
  (let ((result1 (outer 1)))
    ;; Nested labels with CAPTURE (like concat does with vec)
    (let ((captured-var 100))
      (labels ((inner-outer (m)
                 (if (= m 0)
                     captured-var  ; CAPTURE from outer let
                     (labels ((inner-inner (k)
                                (if (= k 0)
                                    captured-var  ; Also CAPTURE
                                    (inner-inner (- k 1)))))
                       (+ (inner-inner 1) (inner-outer (- m 1)))))))
        (let ((result2 (inner-outer 1)))
          (sys-write 1 (number-to-string (+ result1 result2)) 3)
          (sys-write 1 "\n" 1)
          (sys-exit 42))))))
