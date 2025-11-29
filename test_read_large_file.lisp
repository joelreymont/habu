;;; Test native-read-file-large with a moderately sized file

(let ((result (native-read-file-large "/tmp/test_large.txt")))
  (sys-write 1 "Read " 5)
  (sys-write 1 (number-to-string (string-length result))
             (string-length (number-to-string (string-length result))))
  (sys-write 1 " bytes\n" 7)
  (sys-exit (if (> (string-length result) 1000) 42 1)))
