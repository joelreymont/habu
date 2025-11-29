;;; Simplest test: labels with make-vector in argument

(labels ((f (v n)
           (if (= n 0)
               42
               (f (make-vector 1) (- n 1)))))
  (sys-exit (f (make-vector 1) 1)))
