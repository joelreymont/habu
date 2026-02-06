(load "lib/stdlib.habu")
(defun nth-1-body (x)
  (loop
      for e in x
       and i from 0
       count (not (eqt e (nth i x)))))
(format t "ok~%")
(quit)
