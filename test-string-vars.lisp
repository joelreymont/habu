;; Test 2: Variables holding string literals
(let ((s1 "test")
      (s2 "test"))
  (if (string= s1 s2) 2 0))
