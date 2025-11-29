;;; Test if extracted values work with simple labels (not concat)

(labels ((build-list ()
           (list (cons "A" nil) 1)))  ; Return list
  (let* ((result-list (build-list))
         (chunks (car result-list))
         (total (car (cdr result-list))))
    ;; Now use extracted values in ANOTHER labels function
    (labels ((use-vals (c t-val)
               (if (null c)
                   t-val
                   (+ t-val 1))))
      (let ((result (use-vals chunks total)))
        (sys-write 1 (number-to-string result) (string-length (number-to-string result)))
        (sys-write 1 "\n" 1)
        (sys-exit 42)))))
