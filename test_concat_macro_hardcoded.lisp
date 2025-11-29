;;; Test concat-string-list MACRO with hardcoded chunks

(let* ((chunks (cons "Test" nil))
       (total 4))
  (let ((result (concat-string-list chunks total)))
    (sys-write 1 result (string-length result))
    (sys-write 1 "\n" 1)
    (sys-exit 42)))
