;;; Test: compute length without using the string value itself

(labels ((f (s n)
           (if (= n 0)
               (string-length s)
               (f (string-append s "X") (- n 1)))))
  (let ((len (f "" 1)))
    (sys-write 1 "Length: " 8)
    (if (= len 0)
        (sys-write 1 "0" 1)
        (sys-write 1 "1" 1))
    (sys-write 1 "\n" 1)
    (sys-exit 42)))
