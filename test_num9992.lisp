;;; Test number-to-string with 4-digit number

(let ((s (number-to-string 9992)))
  (sys-write 1 "Result: " 8)
  (sys-write 1 s (string-length s))
  (sys-write 1 "\n" 1)
  (sys-exit (if (= (string-length s) 4) 42 1)))
