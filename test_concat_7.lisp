;;; Test concat-string-list with 7 strings

(let ((strings (cons "One" (cons "Two" (cons "Three" (cons "Four" (cons "Five" (cons "Six" (cons "Seven" nil)))))))))
  (let ((result (concat-string-list strings 27)))  ;; 3+3+5+4+4+3+5 = 27
    (sys-write 1 "Result: " 8)
    (sys-write 1 result (string-length result))
    (sys-write 1 "\n" 1)
    (sys-exit (if (= (string-length result) 27) 42 1))))
