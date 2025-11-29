;;; Test labels recursion with string-append in let binding

(labels ((f (s n)
           (if (= n 0)
               s
               (let ((next-s (string-append s "X")))
                 (f next-s (- n 1))))))
  (let ((result (f "" 1)))
    (sys-write 1 "Result: " 8)
    (sys-write 1 result (string-length result))
    (sys-write 1 "\n" 1)
    (if (= (string-length result) 1)
        (sys-exit 42)
        (sys-exit 1))))
