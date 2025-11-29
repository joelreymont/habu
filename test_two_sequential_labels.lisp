;;; Test: two sequential labels, both with funcalls

(labels ((first-rec (n)
           (if (= n 0)
               10
               (first-rec (- n 1)))))
  (let ((result1 (first-rec 1)))
    (labels ((second-rec (m)
               (if (= m 0)
                   20
                   (second-rec (- m 1)))))
      (let ((result2 (second-rec 1)))
        (sys-write 1 (number-to-string (+ result1 result2)) 2)
        (sys-write 1 "\n" 1)
        (sys-exit 42)))))
