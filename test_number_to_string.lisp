;;; Test number-to-string

(let ((s (number-to-string 42)))
  (sys-write 1 "Result: " 8)
  (sys-write 1 s (string-length s))
  (sys-write 1 "\n" 1)
  (sys-exit (if (= (string-length s) 2) 42 1)))
