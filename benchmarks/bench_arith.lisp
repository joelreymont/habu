;;; Arithmetic benchmark - tests +, -, *, basic operations
(defun collatz-steps (n)
  (if (= n 1)
      0
      (if (= (mod n 2) 0)
          (+ 1 (collatz-steps (/ n 2)))
          (+ 1 (collatz-steps (+ (* 3 n) 1))))))

(defun sum-collatz (start end)
  (if (> start end)
      0
      (+ (collatz-steps start) (sum-collatz (+ start 1) end))))

;; Sum collatz steps for numbers 1 to 100
;; This exercises arithmetic and recursion
(mod (sum-collatz 1 100) 256)
