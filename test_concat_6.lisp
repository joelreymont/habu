;;; Test concat-string-list with 6 strings

(let ((strings (cons "One" (cons "Two" (cons "Three" (cons "Four" (cons "Five" (cons "Six" nil))))))))
  (let ((result (concat-string-list strings 22)))  ;; 3+3+5+4+4+3 = 22
    (sys-write 1 "Result: " 8)
    (sys-write 1 result (string-length result))
    (sys-write 1 "\n" 1)
    (sys-exit (if (= (string-length result) 22) 42 1))))
