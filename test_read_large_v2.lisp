;;; Test native-read-file-large v2 (list-based)

;; Test 1: Read a small file
(native-write-file "/tmp/test_v2.txt" "Hello from V2")

(let ((content (native-read-file-large "/tmp/test_v2.txt")))
  (sys-write 1 "Read: " 6)
  (sys-write 1 content (string-length content))
  (sys-write 1 "\n" 1)
  (sys-exit 42))
