;;; Test concat-string-list directly

(let ((lst (cons "A" (cons "B" (cons "C" nil)))))
  (let ((result (concat-string-list lst 3)))
    (sys-write 1 result (string-length result))
    (sys-write 1 "\n" 1)
    (if (= (string-length result) 3)
        (sys-exit 42)
        (sys-exit 1))))
