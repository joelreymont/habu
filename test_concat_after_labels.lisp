;;; Test concat-string-list AFTER labels returns

(let ((result-chunks nil)
      (result-total 0))
  (labels ((collect (n acc total)
             (if (= n 0)
                 (progn (setq result-chunks acc)
                        (setq result-total total))
                 (let* ((chunk "X")
                        (next-acc (cons chunk acc))
                        (next-total (+ total 1)))
                   (collect (- n 1) next-acc next-total)))))
    (collect 5 nil 0))
  ;; Now call concat-string-list AFTER labels has returned
  (let ((result (concat-string-list result-chunks result-total)))
    (sys-write 1 "Result: " 8)
    (sys-write 1 result (string-length result))
    (sys-write 1 "\n" 1)
    (sys-exit (if (= (string-length result) 5) 42 1))))
