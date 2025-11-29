;;; Minimal reverse test

(let* ((lst (cons 1 (cons 2 nil)))
       (rev (reverse lst)))
  (sys-exit (if (= (car rev) 2) 42 1)))
