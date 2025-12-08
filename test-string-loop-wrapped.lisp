(defun string=-loop (s1 s2 len i)
  (if (>= i len)
      t
      (if (= (string-ref s1 i) (string-ref s2 i))
          (string=-loop s1 s2 len (+ i 1))
          nil)))

(defun test-loop ()
  (string=-loop "abc" "abc" 3 0))

;; Call wrapper function
(if (test-loop) 42 99)
