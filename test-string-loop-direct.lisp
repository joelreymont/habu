(defun string=-loop (s1 s2 len i)
  (if (>= i len)
      t
      (if (= (string-ref s1 i) (string-ref s2 i))
          (string=-loop s1 s2 len (+ i 1))
          nil)))

;; Call string=-loop directly
(if (string=-loop "abc" "abc" 3 0) 42 99)
