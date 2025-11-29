;;; Test concat-string-list with 4 strings

(let ((strings (cons "One" (cons "Two" (cons "Three" (cons "Four" nil))))))
  (let ((result (concat-string-list strings)))
    (sys-write 1 "Result: " 8)
    (sys-write 1 result (string-length result))
    (sys-write 1 "\n" 1)
    (sys-exit (if (= (string-length result) 14) 42 1))))
