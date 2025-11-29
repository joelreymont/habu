;;; Test concat-string-list with many strings

(let ((strings (cons "Hello" (cons " " (cons "World" (cons "!" (cons " " (cons "Test" (cons " " (cons "String" nil))))))))))
  (let ((result (concat-string-list strings 24)))  ;; "Hello" + " " + "World" + "!" + " " + "Test" + " " + "String" = 5+1+5+1+1+4+1+6 = 24
    (sys-write 1 "Result: " 8)
    (sys-write 1 result (string-length result))
    (sys-write 1 "\n" 1)
    (sys-exit (if (= (string-length result) 24) 42 1))))
