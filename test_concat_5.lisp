;;; Test concat-string-list with 5 strings

(let ((strings (cons "One" (cons "Two" (cons "Three" (cons "Four" (cons "Five" nil)))))))
  (let ((result (concat-string-list strings 19)))  ;; 3+3+5+4+4 = 19
    (sys-write 1 "Result: " 8)
    (sys-write 1 result (string-length result))
    (sys-write 1 "\n" 1)
    (sys-exit (if (= (string-length result) 19) 42 1))))
