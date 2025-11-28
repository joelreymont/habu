;;; Test string-append with empty strings

;; Test appending empty to empty
(let ((result (string-append "" "")))
  (sys-write 1 "Empty+Empty length: " 20)
  (if (= (string-length result) 0)
      (sys-write 1 "OK\n" 3)
      (sys-write 1 "FAIL\n" 5)))

;; Test appending to empty
(let ((result (string-append "" "Hi")))
  (sys-write 1 "Result: " 8)
  (sys-write 1 result (string-length result))
  (sys-write 1 "\n" 1)
  (sys-exit 42))
