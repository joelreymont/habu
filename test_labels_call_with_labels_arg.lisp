;;; Test: labels funcall, then labels call with another labels as argument

(labels ((outer (n)
           (if (= n 0)
               10
               (outer (- n 1)))))
  (let ((result1 (outer 1)))
    ;; Second labels, then call it with another labels as argument
    (labels ((process (lst)
               (if (null lst)
                   42
                   (+ (car lst) 100))))
      (let ((result2 (process
                       ;; Third labels created AS ARGUMENT!
                       (labels ((make-list (n)
                                  (if (= n 0)
                                      nil
                                      (cons n (make-list (- n 1))))))
                         (make-list 2)))))
        (sys-write 1 (number-to-string (+ result1 result2)) 3)
        (sys-write 1 "\n" 1)
        (sys-exit 42)))))
