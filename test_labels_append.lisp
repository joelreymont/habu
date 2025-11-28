;;; Test labels with string-append

(labels ((append-n (s n)
           (if (= n 0)
               s
               (append-n (string-append s "X") (- n 1)))))
  (let ((result (append-n "" 5)))
    (sys-write 1 result (string-length result))
    (sys-write 1 "\n" 1)
    (if (= (string-length result) 5)
        (sys-exit 42)
        (sys-exit 1))))
