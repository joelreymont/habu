;; Test nested let-inside-cond
;; Pattern similar to compile-expr-full

(defun foo (x)
  (let ((outer 10))
    (cond
      ((= x 0)
       (let ((a (cons 1 2)))
         (cond
           ((= outer 10) (let ((inner (car a))) inner))
           (t 0))))
      (t
       (let ((b (cons 3 4)))
         (cond
           ((= outer 10) (let ((inner (car b))) inner))
           (t 0)))))))

(sys-exit (foo 0))
