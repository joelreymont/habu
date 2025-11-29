;;; Test concat-string-list with two strings

(let* ((s1 "Hello")
       (s2 "World")
       (chunks (cons s2 (cons s1 nil)))  ; Reversed: "World" "Hello"
       (total (+ (string-length s1) (string-length s2))))
  (let ((result (concat-string-list chunks total)))
    (sys-write 1 "Result: " 8)
    (sys-write 1 result (string-length result))
    (sys-write 1 "\n" 1)
    (sys-exit (if (= (string-length result) 10) 42 1))))
