;;; Minimal nested labels test - no strings, just numbers

(labels ((outer (n)
           (labels ((inner (i)
                      (if (= i 0) n (inner (- i 1)))))
             (if (= n 0)
                 0
                 (let ((result (inner 3)))
                   (outer (- n 1)))))))
  (sys-exit (+ 42 (outer 2))))
