;;; Test with actual string-append macro

(labels ((f (s n)
           (if (= n 0)
               (string-length s)
               (f (string-append s "X") (- n 1)))))
  (sys-exit (if (= (f "" 2) 2) 42 1)))
