;;; Test labels with vector allocation

(labels ((f (n)
           (if (= n 0)
               0
               (let ((v (make-vector 10)))
                 (f (- n 1))))))
  (let ((result (f 3)))
    (sys-exit 42)))
