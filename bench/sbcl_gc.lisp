;;;; GC stress benchmark — SBCL side
;;;; Run: sbcl --script bench/sbcl_gc.lisp [--json] [--iters=N] [--live-mb=N]

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defparameter *json-mode* nil)
(defparameter *iters* 100)
(defparameter *live-mb* 8)

(dolist (arg (cdr sb-ext:*posix-argv*))
  (cond
    ((string= arg "--json") (setf *json-mode* t))
    ((and (> (length arg) 8) (string= (subseq arg 0 8) "--iters="))
     (setf *iters* (max 1 (parse-integer (subseq arg 8)))))
    ((and (> (length arg) 10) (string= (subseq arg 0 10) "--live-mb="))
     (setf *live-mb* (max 1 (parse-integer (subseq arg 10)))))))

(defun now-ns ()
  (round (* (get-internal-real-time)
            (/ 1000000000 internal-time-units-per-second))))

(defun percentile-value (values pct)
  (let* ((copy (copy-seq values))
         (n (length copy)))
    (sort copy #'<)
    (elt copy (floor (* (- n 1) pct) 100))))

(defun make-live-root (cons-count)
  (let ((xs nil))
    (dotimes (i cons-count xs)
      (push i xs))))

(defun alloc-ephemeral (cons-count vec-count)
  (let ((xs nil)
        (v nil))
    (dotimes (i cons-count)
      (push i xs))
    (dotimes (i vec-count)
      (setf v (make-array 256 :initial-element i)))
    (values xs v)))

(let* ((live-bytes (* *live-mb* 1024 1024))
       (cons-bytes 16)
       (cons-count (max 1 (floor live-bytes cons-bytes)))
       (vec-count (max 32 (floor cons-count 512)))
       (root (make-live-root cons-count))
       (pauses (make-array *iters* :element-type '(unsigned-byte 64))))
  (declare (ignore root))

  ;; Warmup
  (multiple-value-bind (x y) (alloc-ephemeral cons-count vec-count)
    (declare (ignore x y)))
  (sb-ext:gc :full nil)

  (dotimes (i *iters*)
    (multiple-value-bind (x y) (alloc-ephemeral cons-count vec-count)
      (declare (ignore x y)))
    (let ((t0 (now-ns)))
      (sb-ext:gc :full nil)
      (setf (aref pauses i) (- (now-ns) t0))))

  (let* ((sum 0)
         (max-pause 0)
         (min-pause most-positive-fixnum))
    (dotimes (i *iters*)
      (let ((ns (aref pauses i)))
        (incf sum ns)
        (setf max-pause (max max-pause ns))
        (setf min-pause (min min-pause ns))))
    (let* ((avg (floor sum *iters*))
           (p50 (percentile-value pauses 50))
           (p95 (percentile-value pauses 95))
           (p99 (percentile-value pauses 99)))
      (if *json-mode*
          (format t
                  "{\"engine\":\"sbcl\",\"iters\":~d,\"live_mb\":~d,\"cons_count\":~d,\"vec_count\":~d,\"avg_pause_ns\":~d,\"p50_pause_ns\":~d,\"p95_pause_ns\":~d,\"p99_pause_ns\":~d,\"min_pause_ns\":~d,\"max_pause_ns\":~d}~%"
                  *iters* *live-mb* cons-count vec-count avg p50 p95 p99 min-pause max-pause)
          (progn
            (format t "SBCL GC benchmark~%")
            (format t "  iters: ~d, live_mb: ~d~%" *iters* *live-mb*)
            (format t "  pause avg: ~,3f ms~%" (/ avg 1e6))
            (format t "  pause p50: ~,3f ms~%" (/ p50 1e6))
            (format t "  pause p95: ~,3f ms~%" (/ p95 1e6))
            (format t "  pause p99: ~,3f ms~%" (/ p99 1e6))
            (format t "  pause min/max: ~,3f / ~,3f ms~%" (/ min-pause 1e6) (/ max-pause 1e6)))))))
