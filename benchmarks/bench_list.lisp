;;; List operations benchmark - tests cons, car, cdr, length
(defun make-list-n (n)
  (if (= n 0)
      nil
      (cons n (make-list-n (- n 1)))))

(defun sum-list (lst)
  (if (null lst)
      0
      (+ (car lst) (sum-list (cdr lst)))))

(defun reverse-list (lst acc)
  (if (null lst)
      acc
      (reverse-list (cdr lst) (cons (car lst) acc))))

;; Create list of 1000 elements, sum it, reverse it, sum again
(let* ((lst (make-list-n 1000))
       (s1 (sum-list lst))
       (rev (reverse-list lst nil))
       (s2 (sum-list rev)))
  ;; Both sums should be equal: 1+2+...+1000 = 500500
  ;; Return something reasonable for exit code (mod 256)
  (mod s1 256))
