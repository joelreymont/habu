;;; Test concat-string-list with 4 strings - WITH total length

(let ((strings (cons "One" (cons "Two" (cons "Three" (cons "Four" nil))))))
  (let ((result (concat-string-list strings 15)))  ;; One=3, Two=3, Three=5, Four=4 = 15
    (sys-write 1 "Result: " 8)
    (sys-write 1 result (string-length result))
    (sys-write 1 "\n" 1)
    (sys-exit (if (= (string-length result) 15) 42 1))))
