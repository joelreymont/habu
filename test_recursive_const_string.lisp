;;; Test recursive with constant string (no heap allocation)

(labels ((build (n)
           (if (= n 0)
               "XXX"
               (build (- n 1)))))
  (let ((result (build 3)))
    (sys-write 1 result (string-length result))
    (sys-write 1 "\n" 1)
    (sys-exit (if (= (string-length result) 3) 42 1))))
