;;; Test recursive with let* workaround

(labels ((build-string (n acc)
           (if (= n 0)
               acc
               (let ((next (string-append acc "X")))
                 (build-string (- n 1) next)))))
  (let ((result (build-string 3 "")))
    (sys-write 1 "Result: " 8)
    (sys-write 1 result (string-length result))
    (sys-write 1 "\n" 1)
    (sys-exit (if (= (string-length result) 3) 42 1))))
