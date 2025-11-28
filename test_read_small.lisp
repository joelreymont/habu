;;; Test native-read-file-large with small file

;; Create a test file first
(native-write-file "/tmp/test_tiny.txt" "Hi")

;; Read it back
(let ((content (native-read-file-large "/tmp/test_tiny.txt")))
  (sys-write 1 "Read: " 6)
  (sys-write 1 content (string-length content))
  (sys-write 1 "\n" 1)
  (sys-exit 42))
