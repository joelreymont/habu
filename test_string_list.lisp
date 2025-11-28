;;; Test building a list of strings

(let ((lst (cons "A" (cons "B" (cons "C" nil)))))
  (let ((first (car lst)))
    (sys-write 1 first (string-length first))
    (sys-write 1 "\n" 1)
    (sys-exit 42)))
