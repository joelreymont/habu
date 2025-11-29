;;; Test concat-string-list with single string

(let* ((s1 "Hello")
       (chunks (cons s1 nil))
       (total (string-length s1)))
  (let ((result (concat-string-list chunks total)))
    (sys-write 1 result (string-length result))
    (sys-write 1 "\n" 1)
    (sys-exit (if (= (string-length result) 5) 42 1))))
