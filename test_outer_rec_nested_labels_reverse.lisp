;;; Test: outer-rec, then NESTED labels with reverse arg

(labels ((outer-rec (n acc)
           (if (= n 0)
               (list acc n)
               (outer-rec (- n 1) (+ acc n)))))
  (let* ((result-list (outer-rec 3 0))
         (acc-val (car result-list)))
    ;; NESTED labels like concat has
    (labels ((outer-fn (lst offset)
               (if (null lst)
                   42
                   (labels ((inner-fn (i)
                              (if (< i 2)
                                  (+ i offset)
                                  0)))
                     (+ (inner-fn 0) (outer-fn (cdr lst) (+ offset 1)))))))
      ;; Call with reverse as argument
      (let* ((chunks (cons 1 (cons 2 nil)))
             (result (outer-fn (reverse chunks) 0)))
        (sys-write 1 (number-to-string result) 2)
        (sys-write 1 "\n" 1)
        (sys-exit 42)))))
