;;; Test concat-string-list with empty list

(let ((result (concat-string-list nil 0)))
  (sys-exit (if (= (string-length result) 0) 42 1)))
