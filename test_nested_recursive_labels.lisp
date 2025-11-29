;;; Test: nested recursive labels (like concat has)

(labels ((outer (n acc)
           (if (= n 0)
               acc
               (labels ((inner (m sum)
                          (if (= m 0)
                              sum
                              (inner (- m 1) (+ sum 1)))))
                 (let ((inner-result (inner 2 0)))
                   (outer (- n 1) (+ acc inner-result)))))))
  (let ((result (outer 2 0)))
    (sys-write 1 (number-to-string result) (string-length (number-to-string result)))
    (sys-write 1 "\n" 1)
    (sys-exit 42)))
