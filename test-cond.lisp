;; Test COND implementation
(let ((x 5))
  (cond
    ((= x 1) 10)
    ((= x 2) 20)
    ((= x 5) 50)
    (t 99)))
