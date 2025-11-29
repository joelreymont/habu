;;; Test native-read-file-large with the fix

(let ((content (native-read-file-large "/tmp/small.txt")))
  (sys-write 1 content (string-length content))
  (sys-write 1 "\n" 1)
  (sys-exit 42))
