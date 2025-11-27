;;; Fibonacci benchmark - tests recursive function call overhead
(defun fib (n)
  (if (<= n 1)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

;; fib(30) = 832040, runs in reasonable time
(fib 30)
