;;; Test labels with cons in recursive argument (simpler than string-append)

(labels ((f (pair n)
           (if (= n 0)
               (car pair)
               (f (cons (+ (car pair) 1) 0) (- n 1)))))
  (let ((result (f (cons 0 0) 1)))
    (sys-exit (if (= result 1) 42 1))))
