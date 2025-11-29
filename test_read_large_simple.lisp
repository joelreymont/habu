;;; Test native-read-file-large with simple file

(let ((result (native-read-file-large "/tmp/test_simple.txt")))
  (sys-write 1 "Read: " 6)
  (sys-write 1 result (string-length result))
  (sys-write 1 "\nLength: " 9)
  (sys-write 1 (number-to-string (string-length result))
             (string-length (number-to-string (string-length result))))
  (sys-write 1 "\n" 1)
  (sys-exit (if (= (string-length result) 114) 42 1)))
