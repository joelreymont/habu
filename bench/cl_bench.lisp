;;;; CL comparison bench (SBCL side)
;;;; Run: sbcl --script bench/cl_bench.lisp [--json]

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defun fixnum-loop ()
  "Sum 0 to 10,000,000"
  (declare (optimize (speed 3) (safety 0)))
  (let ((acc 0))
    (declare (type fixnum acc))
    (dotimes (i 10000000 acc)
      (declare (type fixnum i))
      (incf acc i))))

(defun fib (n)
  (declare (type fixnum n)
           (optimize (speed 3) (safety 0)))
  (if (<= n 1) n
      (the fixnum (+ (fib (the fixnum (- n 1)))
                     (fib (the fixnum (- n 2)))))))

(defun tak (x y z)
  (declare (type fixnum x y z)
           (optimize (speed 3) (safety 0)))
  (if (<= x y) z
      (tak (tak (the fixnum (1- x)) y z)
           (tak (the fixnum (1- y)) z x)
           (tak (the fixnum (1- z)) x y))))

(defun list-length-bench ()
  "Build and measure length of 1M-element list"
  (declare (optimize (speed 3) (safety 0)))
  (let ((xs nil))
    (dotimes (i 1000000)
      (push i xs))
    (length xs)))

(defun cons-alloc ()
  "Create 1M cons cells"
  (declare (optimize (speed 3) (safety 0)))
  (let ((xs nil))
    (dotimes (i 1000000)
      (push i xs))
    xs))

(defvar *json-mode* nil)
(defvar *iters* 3)

(dolist (arg (cdr sb-ext:*posix-argv*))
  (cond
    ((string= arg "--json") (setf *json-mode* t))
    ((and (> (length arg) 8) (string= (subseq arg 0 8) "--iters="))
     (setf *iters* (parse-integer (subseq arg 8))))))

(defun bench (name fn iters)
  (funcall fn) ; warmup
  (let ((t0 (get-internal-real-time)))
    (dotimes (i iters)
      (funcall fn))
    (let* ((t1 (get-internal-real-time))
           (ns (round (* (- t1 t0) (/ 1000000000 internal-time-units-per-second)) iters)))
      (list name ns))))

(let ((results
        (list
         (bench "fixnum_loop" #'fixnum-loop *iters*)
         (bench "fib35" (lambda () (fib 35)) *iters*)
         (bench "tak" (lambda () (tak 18 12 6)) *iters*)
         (bench "list_length" #'list-length-bench *iters*)
         (bench "cons_alloc" #'cons-alloc *iters*))))
  (if *json-mode*
      (progn
        (format t "{\"engine\":\"sbcl\",\"benches\":[")
        (loop for (name ns) in results
              for first = t then nil
              do (unless first (format t ","))
                 (format t "{\"name\":\"~a\",\"ns\":~d}" name ns))
        (format t "]}~%"))
      (progn
        (format t "CL comparison bench (SBCL)~%")
        (format t "  iters: ~d~%" *iters*)
        (dolist (r results)
          (format t "  ~a: ~,3f ms~%" (first r) (/ (second r) 1e6))))))
