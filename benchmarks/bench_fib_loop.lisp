;;; Fibonacci benchmark with internal loop to amortize startup
(defun fib (n)
  (if (<= n 1)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

;; Run fib(30) 10 times to amortize startup cost
(defun run-bench (count acc)
  (if (= count 0)
      acc
      (run-bench (- count 1) (+ acc (fib 30)))))

(mod (run-bench 10 0) 256)
