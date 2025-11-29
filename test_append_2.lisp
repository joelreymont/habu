;;; Test string-append with 2 strings directly

(let ((result (string-append "One" "Two")))
  (sys-write 1 "Result: " 8)
  (sys-write 1 result (string-length result))
  (sys-write 1 "\n" 1)
  (sys-exit (if (= (string-length result) 6) 42 1)))
