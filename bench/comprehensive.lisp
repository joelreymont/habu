;;;; Comprehensive CL Benchmark Suite — SBCL side
;;;; Run: sbcl --script bench/comprehensive.lisp [--json]
;;;;
;;;; Categories:
;;;;   arith    — integer/fixnum arithmetic
;;;;   float    — floating-point math
;;;;   recurse  — recursive algorithms
;;;;   list     — list construction/traversal
;;;;   hof      — higher-order functions
;;;;   hash     — hash table operations
;;;;   string   — string manipulation
;;;;   sort     — sorting algorithms
;;;;   gc       — allocation stress
;;;;   symbol   — symbol/package operations

(declaim (optimize (speed 3) (safety 0) (debug 0)))

;;; ========== arith ==========

(defun bench-fixnum-loop ()
  "Sum 0..1M"
  (let ((acc 0))
    (declare (type fixnum acc))
    (dotimes (i 1000000 acc)
      (declare (type fixnum i))
      (incf acc i))))

(defun bench-fixnum-mul ()
  "Multiply-accumulate loop 1M"
  (let ((acc 1))
    (declare (type fixnum acc))
    (dotimes (i 1000000 acc)
      (declare (type fixnum i))
      (setf acc (logand (the fixnum (+ acc (the fixnum (* (the fixnum (1+ i)) 3)))) #xffffff)))))

(defun bench-gcd ()
  "GCD of many pairs"
  (let ((sum 0))
    (declare (type fixnum sum))
    (dotimes (i 100000 sum)
      (declare (type fixnum i))
      (incf sum (gcd (the fixnum (+ i 17)) (the fixnum (+ i 31)))))))

;;; ========== float ==========

(defun bench-float-sum ()
  "Sum 100K floats"
  (let ((acc 0.0d0))
    (declare (type double-float acc))
    (dotimes (i 100000)
      (incf acc (the double-float (* (coerce i 'double-float) 0.001d0))))
    (round acc)))

(defun bench-float-sqrt ()
  "Square-root 100K doubles"
  (let ((acc 0.0d0))
    (declare (type double-float acc))
    (dotimes (i 100000)
      (incf acc (sqrt (the double-float (+ 1.0d0 (coerce i 'double-float))))))
    (round acc)))

;;; ========== recurse ==========

(defun fib (n)
  (declare (type fixnum n)
           (optimize (speed 3) (safety 0)))
  (if (<= n 1) n
      (the fixnum (+ (fib (the fixnum (- n 1)))
                     (fib (the fixnum (- n 2)))))))

(defun bench-fib ()
  (declare (optimize (speed 3) (safety 0)))
  (fib 30))

(defun tak (x y z)
  (declare (type fixnum x y z)
           (optimize (speed 3) (safety 0)))
  (if (<= x y) z
      (tak (tak (the fixnum (1- x)) y z)
           (tak (the fixnum (1- y)) z x)
           (tak (the fixnum (1- z)) x y))))

(defun bench-tak () (tak 18 12 6))

(defun ack (m n)
  (declare (type fixnum m n)
           (optimize (speed 3) (safety 0)))
  (cond ((zerop m) (the fixnum (1+ n)))
        ((zerop n) (ack (the fixnum (1- m)) 1))
        (t (ack (the fixnum (1- m)) (ack m (the fixnum (1- n)))))))

(defun bench-ack () (ack 3 5))

(defun nqueens (n)
  "Count solutions to N-queens"
  (declare (type fixnum n))
  (let ((count 0))
    (declare (type fixnum count))
    (labels ((safe-p (col placed row)
               (declare (type fixnum col row))
               (if (null placed) t
                   (let ((c (car placed)))
                     (declare (type fixnum c))
                     (and (/= c col)
                          (/= (abs (- c col)) row)
                          (safe-p col (cdr placed) (1+ row))))))
             (solve (row placed)
               (declare (type fixnum row))
               (if (= row n)
                   (incf count)
                   (dotimes (col n)
                     (when (safe-p col placed 1)
                       (solve (1+ row) (cons col placed)))))))
      (solve 0 nil))
    count))

(defun bench-nqueens () (nqueens 10))

;;; ========== list ==========

(defun bench-list-build ()
  "Build 100K-element list"
  (let ((xs nil))
    (dotimes (i 100000)
      (push i xs))
    (length xs)))

(defun bench-list-reverse ()
  "Build and reverse 100K-element list"
  (let ((xs nil))
    (dotimes (i 100000)
      (push i xs))
    (length (nreverse xs))))

(defun bench-list-append ()
  "Append 1K 100-element lists"
  (let ((base (loop for i below 100 collect i))
        (result nil))
    (dotimes (i 1000)
      (declare (ignore i))
      (setf result (append base result)))
    (length result)))

(defun bench-assoc ()
  "Assoc lookup 50K times in 100-element alist"
  (let ((al (loop for i below 100 collect (cons i (* i i))))
        (sum 0))
    (declare (type fixnum sum))
    (dotimes (i 50000)
      (let ((pair (assoc (mod i 100) al)))
        (when pair (incf sum (the fixnum (cdr pair))))))
    sum))

;;; ========== hof ==========

(defun bench-mapcar ()
  "Mapcar over 10K list"
  (let ((xs (loop for i below 10000 collect i)))
    (length (mapcar #'1+ xs))))

(defun bench-reduce ()
  "Reduce + over 10K list"
  (let ((xs (loop for i below 10000 collect i)))
    (reduce #'+ xs)))

(defun bench-remove-if ()
  "Remove-if over 10K list manually"
  (let ((xs (loop for i below 10000 collect i))
        (result nil))
    (dolist (x xs)
      (unless (oddp x)
        (push x result)))
    (length result)))

;;; ========== hash ==========

(defun bench-hash-insert ()
  "Insert 20K entries"
  (let ((h (make-hash-table :size 256)))
    (dotimes (i 20000)
      (setf (gethash i h) i))
    (hash-table-count h)))

(defun bench-hash-lookup ()
  "Lookup 50K times in 20K-entry table"
  (let ((h (make-hash-table :size 256))
        (sum 0))
    (declare (type fixnum sum))
    (dotimes (i 20000)
      (setf (gethash i h) i))
    (dotimes (i 50000)
      (let ((v (gethash (mod i 20000) h)))
        (when v (incf sum (the fixnum v)))))
    sum))

;;; ========== string ==========

(defun bench-string-concat ()
  "Concatenate 1K short strings"
  (let ((result ""))
    (dotimes (i 1000)
      (declare (ignore i))
      (setf result (concatenate 'string result "x")))
    (length result)))

(defun bench-string-search ()
  "Search in 10K-char string 1K times"
  (let ((haystack (make-string 10000 :initial-element #\a))
        (count 0))
    (setf (char haystack 9999) #\b)
    (dotimes (i 1000)
      (declare (ignore i))
      (when (position #\b haystack) (incf count)))
    count))

;;; ========== sort ==========

(defun bench-sort-fixnum ()
  "Sort 100 fixnums (stack-limited to match Habu)"
  (let ((xs (loop for i from 100 downto 1 collect i)))
    (length (sort xs #'<))))

(defun bench-sort-string ()
  "Sort 100 strings (stack-limited to match Habu)"
  (let ((xs (loop for i below 100
                  collect (format nil "~6,'0d" (- 100 i)))))
    (length (sort xs #'string<))))

;;; ========== gc ==========

(defun bench-gc-cons ()
  "Allocate 100K cons cells (ephemeral)"
  (let ((last nil))
    (dotimes (i 100000)
      (setf last (cons i nil)))
    last))

(defun bench-gc-vector ()
  "Allocate 10K small vectors"
  (let ((v nil))
    (dotimes (i 10000)
      (setf v (make-array 4 :initial-element i)))
    (aref v 0)))

;;; ========== symbol ==========

(defun bench-intern ()
  "Intern 10K symbols"
  (let ((pkg (make-package "BENCH-INTERN-TEMP" :use nil))
        (count 0))
    (dotimes (i 10000)
      (intern (format nil "SYM-~d" i) pkg)
      (incf count))
    (delete-package pkg)
    count))

;;; ========== harness ==========

(defvar *json-mode* nil)
(defvar *iters* 3)

(dolist (arg (cdr sb-ext:*posix-argv*))
  (cond
    ((string= arg "--json") (setf *json-mode* t))
    ((and (> (length arg) 8) (string= (subseq arg 0 8) "--iters="))
     (setf *iters* (parse-integer (subseq arg 8))))))

(defun bench (name fn iters)
  (handler-case
      (progn
        (funcall fn) ; warmup
        (let ((t0 (get-internal-real-time)))
          (dotimes (i iters)
            (funcall fn))
          (let* ((t1 (get-internal-real-time))
                 (ns (round (* (- t1 t0) (/ 1000000000 internal-time-units-per-second)) iters)))
            (list name ns nil))))
    (error (e) (list name 0 (format nil "~a" e)))))

(defvar *benches*
  '(;; arith
    ("fixnum_loop"   bench-fixnum-loop)
    ("fixnum_mul"    bench-fixnum-mul)
    ("gcd"           bench-gcd)
    ;; float
    ("float_sum"     bench-float-sum)
    ("float_sqrt"    bench-float-sqrt)
    ;; recurse
    ("fib30"         bench-fib)
    ("tak"           bench-tak)
    ("ack"           bench-ack)
    ("nqueens10"     bench-nqueens)
    ;; list
    ("list_build"    bench-list-build)
    ("list_reverse"  bench-list-reverse)
    ("list_append"   bench-list-append)
    ("assoc"         bench-assoc)
    ;; hof
    ("mapcar"        bench-mapcar)
    ("reduce"        bench-reduce)
    ("remove_if"     bench-remove-if)
    ;; hash
    ("hash_insert"   bench-hash-insert)
    ("hash_lookup"   bench-hash-lookup)
    ;; string
    ("string_concat" bench-string-concat)
    ("string_search" bench-string-search)
    ;; sort
    ("sort_fixnum"   bench-sort-fixnum)
    ("sort_string"   bench-sort-string)
    ;; gc
    ("gc_cons"       bench-gc-cons)
    ("gc_vector"     bench-gc-vector)
    ;; symbol
    ("intern"        bench-intern)))

(let ((results (mapcar (lambda (spec)
                         (bench (first spec) (symbol-function (second spec)) *iters*))
                       *benches*)))
  (if *json-mode*
      (progn
        (format t "{\"engine\":\"sbcl\",\"benches\":[")
        (loop for (name ns err) in results
              for first = t then nil
              do (unless first (format t ","))
                 (if err
                     (format t "{\"name\":\"~a\",\"ns\":0,\"error\":\"~a\"}" name err)
                     (format t "{\"name\":\"~a\",\"ns\":~d}" name ns)))
        (format t "]}~%"))
      (progn
        (format t "~%Comprehensive CL Benchmark (SBCL ~a)~%" (lisp-implementation-version))
        (format t "iters: ~d~%~%" *iters*)
        (format t "~30a ~12a~%" "Benchmark" "Time")
        (format t "~30a ~12a~%" (make-string 30 :initial-element #\-) (make-string 12 :initial-element #\-))
        (dolist (r results)
          (destructuring-bind (name ns err) r
            (if err
                (format t "~30a ~12a~%" name (format nil "ERR: ~a" err))
                (format t "~30a ~10,3f ms~%" name (/ ns 1e6))))))))
