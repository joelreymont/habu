;;; Test labels calling string-append (non-recursive)

(labels ((f (n)
           (if (= n 0)
               "Done"
               (string-append "X" "Y"))))
  (let ((result (f 1)))
    (if (= (string-length result) 2)
        (sys-exit 42)
        (sys-exit 1))))
