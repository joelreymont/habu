;;; Test: outer-rec, then call function with reverse as argument (not concat)

(labels ((outer-rec (n acc)
           (if (= n 0)
               (list acc n)
               (outer-rec (- n 1) (+ acc n)))))
  (let* ((result-list (outer-rec 3 0))
         (acc-val (car result-list)))
    ;; Simple function that takes a list
    (labels ((process-list (lst)
               (if (null lst)
                   0
                   (+ (car lst) (process-list (cdr lst))))))
      ;; Call with reverse as argument (reverse expands to labels!)
      (let* ((chunks (cons 1 (cons 2 (cons 3 nil))))
             (result (process-list (reverse chunks))))
        (sys-write 1 (number-to-string result) 1)
        (sys-write 1 "\n" 1)
        (sys-exit 42)))))
