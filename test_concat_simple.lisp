;;; Test concat-string-list with 2 strings

(let* ((s1 "Hello")
       (s2 "World")
       (result (concat-string-list (cons s2 (cons s1 nil)) 10)))
  (sys-write 1 result (string-length result))
  (sys-write 1 "\n" 1)
  (sys-exit 42))
