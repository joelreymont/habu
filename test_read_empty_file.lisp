;;; Test native-read-file-large with empty file

(let ((result (native-read-file-large "/tmp/test_simple.txt")))
  (sys-write 1 "Length: " 8)
  (sys-write 1 (number-to-string (string-length result))
             (string-length (number-to-string (string-length result))))
  (sys-write 1 "\n" 1)
  (sys-exit 42))
