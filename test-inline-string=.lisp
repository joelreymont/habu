;; Test 2: Inline my-string= definition (expected to work if defun works)
(defun my-string=-loop (s1 s2 len i)
  (if (>= i len)
      t
      (if (= (string-ref s1 i) (string-ref s2 i))
          (my-string=-loop s1 s2 len (+ i 1))
          nil)))

(defun my-string= (s1 s2)
  (let ((len1 (string-length s1))
        (len2 (string-length s2)))
    (if (= len1 len2)
        (my-string=-loop s1 s2 len1 0)
        nil)))

;; Return 42 if strings match, 99 if they don't
(if (my-string= "test" "test") 42 99)
