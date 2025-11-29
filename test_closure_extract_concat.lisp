;;; Test: closure + extract + concat

(let* ((outer-val 100))
  (labels ((build-list (n)
             (if (= n 0)
                 (list (cons "Test" nil) outer-val)  ; Capture outer-val, return list
                 (build-list (- n 1)))))
    (let* ((result-list (build-list 1))
           (chunks (car result-list))
           (total (car (cdr result-list))))
      (let ((result (concat-string-list chunks total)))
        (sys-write 1 result (string-length result))
        (sys-write 1 "\n" 1)
        (sys-exit 42)))))
