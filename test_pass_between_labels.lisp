;;; Test passing value from one labels function to another

(labels ((build-list (n acc)
           (if (= n 0)
               acc
               (build-list (- n 1) (cons n acc)))))
  (let ((my-list (build-list 3 nil)))
    (labels ((process-list (lst)
               (if (null lst)
                   0
                   (+ (car lst) (process-list (cdr lst))))))
      (let ((result (process-list my-list)))
        (sys-write 1 (number-to-string result) (string-length (number-to-string result)))
        (sys-write 1 "\n" 1)
        (sys-exit 42)))))
