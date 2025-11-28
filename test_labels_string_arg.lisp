;;; Test labels with string as recursive argument

(labels ((f (s n)
           (if (= n 0)
               s
               (f "CONST" (- n 1)))))
  (let ((result (f "" 2)))
    (if (= (string-length result) 5)
        (sys-exit 42)
        (sys-exit 1))))
