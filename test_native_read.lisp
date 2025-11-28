;;; Test basic native-read-file

;; Write a test file
(native-write-file "/tmp/test.txt" "Hello from Habu!")

;; Read it back
(let ((content (native-read-file "/tmp/test.txt")))
  (sys-write 1 content (string-length content))
  (sys-write 1 "\n" 1)
  (sys-exit 42))
