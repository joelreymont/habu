;;; Minimal test case for labels + heap allocation bug

;; Test 1: labels with cons (heap allocation)
(labels ((f (n)
           (if (= n 0)
               nil
               (cons n (f (- n 1))))))
  (let ((result (f 3)))
    (if (consp result)
        (sys-exit 42)
        (sys-exit 1))))
