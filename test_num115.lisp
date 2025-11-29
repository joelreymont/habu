;;; Test number-to-string with 115

(let ((s (number-to-string 115)))
  (sys-write 1 "Result: " 8)
  (sys-write 1 s (string-length s))
  (sys-write 1 "\n" 1)
  (sys-exit (if (= (string-length s) 3) 42 1)))
