;;;; Maxima workload benchmark — SBCL side
;;;; Run: sbcl --script bench/maxima_workload.lisp [--json] [--scale=N]

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defparameter *json-mode* nil)
(defparameter *scale* 1)

(dolist (arg (cdr sb-ext:*posix-argv*))
  (cond
    ((string= arg "--json") (setf *json-mode* t))
    ((and (> (length arg) 8) (string= (subseq arg 0 8) "--scale="))
     (setf *scale* (max 1 (parse-integer (subseq arg 8)))))))

(defun now-ns ()
  (round (* (get-internal-real-time)
            (/ 1000000000 internal-time-units-per-second))))

(defun json-escape (s)
  (with-output-to-string (out)
    (loop for ch across s do
      (case ch
        (#\" (write-string "\\\"" out))
        (#\\ (write-string "\\\\" out))
        (#\Newline (write-string "\\n" out))
        (#\Return (write-string "\\r" out))
        (#\Tab (write-string "\\t" out))
        (t (write-char ch out))))))

(load "../maxima/src/maxima-package.lisp")
(load "lib/maxima-stubs.lisp")

(defparameter *maxima-src* "../maxima/src/")
(defparameter *maxima-files*
  '("lmdcls" "letmac" "clmacs" "commac" "mormac" "globals" "compat"
    "defcal" "maxmac" "mopers" "mforma" "mrgmac" "strmac" "rzmac" "ratmac" "mhayat" "combin" "opers"
    "utils" "merror" "mutils" "sumcon" "sublis" "mformt" "outmis" "ar"
    "comm" "comm2" "mlisp" "mmacro" "buildq"
    "simp" "float" "csimp" "csimp2" "zero" "logarc" "rpart"
    "suprv1" "inmis" "db"
    "compar" "lesfac" "factor" "algfac" "nalgfa" "ufact" "ifactor" "rat3a" "rat3b" "rat3c"
    "rat3d" "rat3e" "nrat4" "ratout" "acall"
    "mat" "linnew" "matrix" "sprdet" "newinv" "newdet"
    "schatc" "matcom" "matrun" "nisimp" "nparse" "displm" "displa" "nforma" "grind"
    "nset" "sinint" "sin" "trigi" "trigo" "trgred"
    "tlimit" "limit"
    "solve" "psolve" "algsys" "sqrtdenest" "polyrz" "cpoly"))

(defun load-maxima-workload ()
  (let ((ok 0)
        (total (length *maxima-files*))
        (attempted 0)
        (fail 0)
        (first-error nil)
        (t0 (now-ns)))
    (dolist (name *maxima-files*)
      (incf attempted)
      (handler-bind ((warning #'muffle-warning)
                     (style-warning #'muffle-warning))
        (handler-case
            (progn
              (load (concatenate 'string *maxima-src* name ".lisp"))
              (incf ok))
          (error (e)
            (setf fail 1)
            (setf first-error e)
            (return)))))
    (values ok total fail attempted first-error (- (now-ns) t0))))

(defun bench-simplifya (n)
  (let ((out nil))
    (dotimes (i n out)
      (declare (ignore i))
      (setf out (maxima::simplifya '((maxima::mplus) 3 4) t)))))

(defun bench-diff (n)
  (let ((out nil))
    (dotimes (i n out)
      (declare (ignore i))
      (setf out (maxima::$diff 0 'maxima::$x)))))

(defun bench-integrate (n)
  (let ((out nil))
    (dotimes (i n out)
      (declare (ignore i))
      (setf out (maxima::$integrate 0 'maxima::$x)))))

(defun bench-factor (n)
  (let ((out nil))
    (dotimes (i n out)
      (declare (ignore i))
      (setf out (maxima::$factor 1)))))

(defun bench-ratsimp (n)
  (let ((out nil))
    (dotimes (i n out)
      (declare (ignore i))
      (setf out (maxima::$ratsimp 1)))))

(defun bench-limit (n)
  (let ((out nil))
    (dotimes (i n out)
      (declare (ignore i))
      (setf out (maxima::$limit 0 'maxima::$x 0)))))

(defun bench-solve (n)
  (let ((out nil))
    (dotimes (i n out)
      (declare (ignore i))
      (setf out (maxima::$solve 0 'maxima::$x)))))

(defun bench-determinant (n)
  (let ((out nil))
    (dotimes (i n out)
      (declare (ignore i))
      (setf out (maxima::$determinant 1)))))

(defun bench-expand (n)
  (let ((out nil))
    (dotimes (i n out)
      (declare (ignore i))
      (setf out (maxima::$expand 1)))))

(defun bench-sin (n)
  (let ((out nil))
    (dotimes (i n out)
      (declare (ignore i))
      (setf out (maxima::$sin 0)))))

(defun bench-cos (n)
  (let ((out nil))
    (dotimes (i n out)
      (declare (ignore i))
      (setf out (maxima::$cos 0)))))

(defparameter *bench-specs*
  '(("simplifya" bench-simplifya 200)
    ("diff" bench-diff 200)
    ("integrate" bench-integrate 200)
    ("factor" bench-factor 200)
    ("ratsimp" bench-ratsimp 200)
    ("limit" bench-limit 20)
    ("solve" bench-solve 20)
    ("determinant" bench-determinant 20)
    ("expand" bench-expand 200)
    ("sin" bench-sin 200)
    ("cos" bench-cos 200)))

(defun run-bench (name fn base-iters)
  (let ((iters (* base-iters *scale*)))
    (handler-case
        (progn
          (funcall fn iters)
          (let ((t0 (now-ns)))
            (funcall fn iters)
            (list name iters (- (now-ns) t0) nil)))
      (error (e)
        (declare (ignore e))
        (list name iters 0 "error")))))

(multiple-value-bind (ok total fail attempted first-error load-ns)
    (load-maxima-workload)
  (let ((results
          (mapcar (lambda (spec)
                    (run-bench (first spec) (symbol-function (second spec)) (third spec)))
                  *bench-specs*)))
    (if *json-mode*
        (progn
          (format t "{\"engine\":\"sbcl\",\"workload\":\"maxima\",\"scale\":~d," *scale*)
          (format t "\"loader\":{\"ok\":~d,\"total\":~d,\"fail\":~d,\"attempted\":~d,\"missing\":0,\"ns\":~d" ok total fail attempted load-ns)
          (when first-error
            (format t ",\"error\":\"load-error\""))
          (format t "},\"benches\":[")
          (loop for (name iters ns err) in results
                for first = t then nil
                do (unless first (format t ","))
                   (if err
                       (format t "{\"name\":\"~a\",\"category\":\"maxima\",\"iters\":~d,\"ns\":0,\"error\":\"~a\"}"
                               name iters (json-escape err))
                       (format t "{\"name\":\"~a\",\"category\":\"maxima\",\"iters\":~d,\"ns\":~d}"
                               name iters ns)))
          (format t "]}~%"))
        (progn
          (format t "Maxima workload benchmark (SBCL ~a)~%" (lisp-implementation-version))
          (format t "  scale: ~d~%" *scale*)
          (format t "  loader: ok=~d/~d fail=~d attempted=~d ~,3f ms~%"
                  ok total fail attempted (/ load-ns 1e6))
          (when first-error
            (format t "  loader-error: load-error~%"))
          (dolist (r results)
            (destructuring-bind (name iters ns err) r
              (if err
                  (format t "  ~a: ERR(~a)~%" name err)
                  (format t "  ~a: ~,3f ms total (~d iters)~%" name (/ ns 1e6) iters))))))))
