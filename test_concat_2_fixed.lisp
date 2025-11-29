;;; Test concat-string-list with 2 strings - WITH total length

(let ((strings (cons "One" (cons "Two" nil))))
  (let ((result (concat-string-list strings 6)))  ;; 3 + 3 = 6
    (sys-write 1 "Result: " 8)
    (sys-write 1 result (string-length result))
    (sys-write 1 "\n" 1)
    (sys-exit (if (= (string-length result) 6) 42 1))))
