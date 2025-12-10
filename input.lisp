(defun map (fn lst)
  (if (null lst)
      nil
      (cons (funcall fn (car lst)) (map fn (cdr lst)))))
(defun double (x) (* x 2))
(car (map double (cons 21 nil)))