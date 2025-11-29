;;; Test simple string-append without recursion

(let ((s1 "Hello")
      (s2 " World"))
  (let ((result (string-append s1 s2)))
    (sys-write 1 result (string-length result))
    (sys-write 1 "\n" 1)
    (sys-exit 42)))
