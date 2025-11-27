;;; SBCL timed fibonacci - measures just computation time
(defun fib (n)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum n))
  (if (<= n 1)
      n
      (the fixnum (+ (fib (- n 1)) (fib (- n 2))))))

(let ((start (get-internal-real-time)))
  (fib 30)
  (let ((end (get-internal-real-time)))
    (format t "Time: ~,3F ms~%"
            (/ (* 1000.0 (- end start)) internal-time-units-per-second))))
