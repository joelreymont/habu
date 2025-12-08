;; Test string-length alone
(let ((len1 (string-length "abc"))
      (len2 (string-length "abc")))
  (if (= len1 len2) 42 99))
