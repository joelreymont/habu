;;; Test reverse with 2 strings

(let ((strings (cons "One" (cons "Two" nil))))
  (let ((reversed (reverse strings)))
    (let ((first (car reversed))
          (second (car (cdr reversed))))
      (sys-write 1 "First: " 7)
      (sys-write 1 first (string-length first))
      (sys-write 1 "\n" 1)
      (sys-write 1 "Second: " 8)
      (sys-write 1 second (string-length second))
      (sys-write 1 "\n" 1)
      (sys-exit 42))))
