;; Test let-inside-cond - should return 3 when x=1

(defun foo (x)
  (cond
    ((= x 0) (let ((a (cons 1 2))) (car a)))
    (t (let ((b (cons 3 4))) (car b)))))

(sys-exit (foo 1))
