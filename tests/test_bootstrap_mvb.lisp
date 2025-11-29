;;; Test multiple values (values, multiple-value-bind) in bootstrap compiler
(load "bootstrap/compiler.lisp")
(in-package :habu)

(defvar *passed* 0)
(defvar *failed* 0)

(defun parse-run-bytecode-output (output)
  "Parse raw result from run-bytecode output"
  (let ((pos (search "Raw result:" output)))
    (when pos
      (let* ((start (+ pos 12))
             (end (or (position #\Space output :start start) (length output)))
             (hex (subseq output start end)))
        (when (and (> (length hex) 2)
                   (string-equal (subseq hex 0 2) "0x"))
          (setf hex (subseq hex 2)))
        (let ((raw (parse-integer hex :radix 16 :junk-allowed t)))
          (when raw
            (if (>= raw (ash 1 63))
                (ash (- raw (ash 1 64)) -4)
                (ash raw -4))))))))

(defun test-case (name source expected)
  (format t "~A: " name)
  (handler-case
    (let* ((code (compile-program (read-all source) nil)))
      (with-open-file (out "/tmp/mvb-test.bin" :direction :output
                           :if-exists :supersede
                           :element-type '(unsigned-byte 8))
        (dolist (byte code) (write-byte byte out)))
      (let* ((output (with-output-to-string (s)
                       (sb-ext:run-program "./run-bytecode" '("/tmp/mvb-test.bin")
                                           :output s :error :output :search nil)))
             (result (parse-run-bytecode-output output)))
        (if (eql result expected)
            (progn
              (format t "PASS (~A)~%" result)
              (incf *passed*))
            (progn
              (format t "FAIL (expected ~A, got ~A)~%" expected result)
              (incf *failed*)))))
    (error (e)
      (format t "ERROR: ~A~%" e)
      (incf *failed*))))

(format t "~%=== Bootstrap Multiple Values Tests ===~%~%")

;; Basic values - single value
(test-case "values-single"
"(values 42)"
42)

;; values with no args
(test-case "values-none"
"(let ((x 10))
   (values)
   x)"
10)

;; multiple-value-bind with single value
(test-case "mvb-single"
"(multiple-value-bind (a) (values 42)
   a)"
42)

;; multiple-value-bind - use first value
(test-case "mvb-first"
"(multiple-value-bind (a b) (values 10 20)
   a)"
10)

;; multiple-value-bind - use second value
(test-case "mvb-second"
"(multiple-value-bind (a b) (values 10 20)
   b)"
20)

;; multiple-value-bind - sum values
(test-case "mvb-sum"
"(multiple-value-bind (a b) (values 10 20)
   (+ a b))"
30)

;; mvb with defun returning values
(test-case "mvb-fn"
"(defun pair (x y) (values x y))
 (multiple-value-bind (a b) (pair 3 4)
   (* a b))"
12)

;; mvb with arithmetic on values
(test-case "mvb-arith"
"(defun divmod (n d)
   (let ((q (/ n d)))
     (let ((r (- n (* q d))))
       (values q r))))
 (multiple-value-bind (q r) (divmod 17 5)
   (+ (* q 10) r))"
32)

(format t "~%=== Results: ~A passed, ~A failed ===~%" *passed* *failed*)
