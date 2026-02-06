(load "lib/stdlib.habu")
(defun nset-exclusive-or-with-check (x y &key (key 'no-key)
                                       test test-not)
  (setf x (copy-list x))
  (setf y (copy-list y))
  (apply #'nset-exclusive-or
         x y
         `(,@(unless (eqt key 'no-key) `(:key ,key))
             ,@(when test `(:test ,test))
             ,@(when test-not `(:test-not ,test-not)))))
(format t "ok~%")
