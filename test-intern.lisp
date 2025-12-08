;; Simplest possible test
(sys-write-string "Test 1\n")
(let ((x 42))
  (sys-write-int x)
  (sys-write-string "\n"))
(sys-write-string "Test 2\n")
