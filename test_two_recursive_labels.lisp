;;; Test: call two different recursive labels functions sequentially

(labels ((first-rec (n acc)
           (if (= n 0)
               acc
               (first-rec (- n 1) (+ acc n)))))
  (let ((result1 (first-rec 3 0)))
    (labels ((second-rec (m acc)
               (if (= m 0)
                   acc
                   (second-rec (- m 1) (* acc 2)))))
      (let ((result2 (second-rec 3 1)))
        (sys-write 1 (number-to-string (+ result1 result2))
                     (string-length (number-to-string (+ result1 result2))))
        (sys-write 1 "\n" 1)
        (sys-exit 42)))))
