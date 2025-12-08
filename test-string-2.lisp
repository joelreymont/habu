(defun string=-loop (s1 s2 len i)
  (if (>= i len)
      t
      (if (= (string-ref s1 i) (string-ref s2 i))
          (string=-loop s1 s2 len (+ i 1))
          nil)))

(defun string= (s1 s2)
  (let ((len1 (string-length s1))
        (len2 (string-length s2)))
    (if (= len1 len2)
        (string=-loop s1 s2 len1 0)
        nil)))

;; Test: Variables - should return 42 if equal, 99 if not
(let ((s1 "abc")
      (s2 "abc"))
  (if (string= s1 s2) 42 99))
