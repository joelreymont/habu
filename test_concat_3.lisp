;;; Test concat-string-list with 3 strings

(let ((strings (cons "One" (cons "Two" (cons "Three" nil)))))
  (let ((result (concat-string-list strings)))
    (sys-write 1 "Result: " 8)
    (sys-write 1 result (string-length result))
    (sys-write 1 "\n" 1)
    (sys-exit (if (= (string-length result) 11) 42 1))))
