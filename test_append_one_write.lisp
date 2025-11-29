;;; Test string-append with just one sys-write

(labels ((f (s n)
           (if (= n 0)
               s
               (f (string-append s "X") (- n 1)))))
  (let ((result (f "" 1)))
    (sys-write 1 result (string-length result))
    (sys-exit 42)))
