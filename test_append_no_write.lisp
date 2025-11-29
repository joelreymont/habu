;;; Test string-append without sys-write, but with result binding

(labels ((f (s n)
           (if (= n 0)
               s
               (f (string-append s "X") (- n 1)))))
  (let ((result (f "" 1)))
    (if (= (string-length result) 1)
        (sys-exit 42)
        (sys-exit 1))))
