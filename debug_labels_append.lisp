;;; Debug version with sys-write at each step

(labels ((f (s n)
           (progn
             (sys-write 1 "f called, n=" 12)
             (sys-write 1 (if (= n 0) "0" (if (= n 1) "1" "2")) 1)
             (sys-write 1 "\n" 1)
             (if (= n 0)
                 s
                 (let ((appended (string-append s "X")))
                   (progn
                     (sys-write 1 "appended: " 10)
                     (sys-write 1 appended (string-length appended))
                     (sys-write 1 "\n" 1)
                     (f appended (- n 1))))))))
  (let ((result (f "" 1)))
    (sys-write 1 "Final result: " 14)
    (sys-write 1 result (string-length result))
    (sys-write 1 "\n" 1)
    (sys-exit 42)))
