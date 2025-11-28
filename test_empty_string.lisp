;;; Test empty string handling

(let ((s ""))
  (sys-write 1 "Empty string length: " 21)
  (if (= (string-length s) 0)
      (sys-write 1 "0\n" 2)
      (sys-write 1 "NOT 0!\n" 7)))

;; Test appending to empty
(let ((s "")
      (s2 "Test"))
  (let ((result (string-append s s2)))
    (sys-write 1 result (string-length result))
    (sys-write 1 "\n" 1)
    (sys-exit 42)))
