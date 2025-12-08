;; Test 4: Two symbol names
(let* ((sym1 (intern "test"))
       (sym2 (intern "test"))
       (name1 (symbol-name sym1))
       (name2 (symbol-name sym2)))
  (if (string= name1 name2) 4 0))
