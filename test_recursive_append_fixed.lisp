;;; Test recursive labels with new string-append

(labels ((build-string (n acc)
           (if (= n 0)
               acc
               (build-string (- n 1) (string-append acc "X")))))
  (let ((result (build-string 3 "")))
    (sys-write 1 "Result: " 8)
    (sys-write 1 result (string-length result))
    (sys-write 1 "\n" 1)
    (if (= (string-length result) 3)
        (sys-exit 42)
        (sys-exit 1))))
