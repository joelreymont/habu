;;; Test labels recursion without string-append

(labels ((count-down (n acc)
           (if (= n 0)
               acc
               (count-down (- n 1) (+ acc 1)))))
  (let ((result (count-down 10 0)))
    (if (= result 10)
        (sys-exit 42)
        (sys-exit 1))))
