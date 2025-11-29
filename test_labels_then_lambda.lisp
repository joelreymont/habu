;;; Test: labels with one funcall, then call a lambda

(labels ((simple-rec (n)
           (if (= n 0)
               42
               (simple-rec (- n 1)))))
  (let ((result1 (simple-rec 1)))
    ;; Now call a simple lambda
    (let ((fn (lambda (x) (+ x 10))))
      (let ((result2 (funcall fn 20)))
        (sys-write 1 (number-to-string (+ result1 result2)) 2)
        (sys-write 1 "\n" 1)
        (sys-exit 42)))))
