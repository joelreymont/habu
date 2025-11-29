;;; Debug version with direct string-append in argument (should crash)

(labels ((f (s n)
           (progn
             (sys-write 1 "f called, n=" 12)
             (sys-write 1 (if (= n 0) "0" (if (= n 1) "1" "2")) 1)
             (sys-write 1 "\n" 1)
             (if (= n 0)
                 s
                 (progn
                   (sys-write 1 "About to call f with string-append\n" 36)
                   (f (string-append s "X") (- n 1)))))))
  (let ((result (f "" 1)))
    (sys-write 1 "Final result: " 14)
    (sys-write 1 result (string-length result))
    (sys-write 1 "\n" 1)
    (sys-exit 42)))
