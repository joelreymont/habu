;; Test string-ref comparison
(let ((c1 (string-ref "abc" 0))
      (c2 (string-ref "abc" 0)))
  (if (= c1 c2) 42 99))
