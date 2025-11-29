;;; Test: recursive call evaluates string-append but returns constant

(labels ((f (n)
           (if (= n 0)
               42
               (progn
                 (string-append "A" "B")  ; evaluate but ignore result
                 (f (- n 1))))))
  (sys-exit (f 1)))
