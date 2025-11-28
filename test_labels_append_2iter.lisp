;;; Test labels calling string-append recursively (just 2 iterations)

(labels ((f (s n)
           (if (= n 0)
               s
               (f (string-append s "X") (- n 1)))))
  (let ((result (f "" 2)))
    (sys-write 1 "Result: " 8)
    (sys-write 1 result (string-length result))
    (sys-write 1 "\n" 1)
    (if (= (string-length result) 2)
        (sys-exit 42)
        (sys-exit 1))))
