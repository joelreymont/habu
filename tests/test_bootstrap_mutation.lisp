;;; Test mutation operations (setq, setcar, setcdr, incf, push, setf) in bootstrap compiler
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
    (let* ((code (nc-compile-program (nc-read-all source) nil)))
      (with-open-file (out "/tmp/mutation-test.bin" :direction :output
                           :if-exists :supersede
                           :element-type '(unsigned-byte 8))
        (dolist (byte code) (write-byte byte out)))
      (let* ((output (with-output-to-string (s)
                       (sb-ext:run-program "./run-bytecode" '("/tmp/mutation-test.bin")
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

(format t "~%=== Bootstrap Mutation Operations Tests ===~%~%")

;; setq - basic variable assignment
(test-case "setq-basic"
"(let ((x 10))
   (setq x 20)
   x)"
20)

(test-case "setq-return"
"(let ((x 10))
   (setq x 42))"
42)

(test-case "setq-nested"
"(let ((x 0))
   (let ((y 5))
     (setq x (+ y 10)))
   x)"
15)

;; incf - increment variable
(test-case "incf-default"
"(let ((x 10))
   (incf x)
   x)"
11)

(test-case "incf-delta"
"(let ((x 10))
   (incf x 5)
   x)"
15)

(test-case "incf-twice"
"(let ((x 0))
   (incf x)
   (incf x)
   x)"
2)

;; push - push onto list variable
(test-case "push-basic"
"(let ((lst nil))
   (push 1 lst)
   (car lst))"
1)

(test-case "push-multiple"
"(let ((lst nil))
   (push 1 lst)
   (push 2 lst)
   (car lst))"
2)

;; setf with car
(test-case "setf-car"
"(let ((x (cons 1 2)))
   (setf (car x) 10)
   (car x))"
10)

;; setf with cdr
(test-case "setf-cdr"
"(let ((x (cons 1 2)))
   (setf (cdr x) 20)
   (cdr x))"
20)

;; setcar/setcdr directly
(test-case "setcar-direct"
"(let ((x (cons 1 2)))
   (setcar x 100)
   (car x))"
100)

(test-case "setcdr-direct"
"(let ((x (cons 1 2)))
   (setcdr x 200)
   (cdr x))"
200)

;; Mutation in functions
(test-case "fn-setq"
"(defun inc-ret (x)
   (setq x (+ x 1))
   x)
 (inc-ret 10)"
11)

(test-case "fn-incf"
"(defun add-three (x)
   (incf x)
   (incf x)
   (incf x)
   x)
 (add-three 0)"
3)

(format t "~%=== Results: ~A passed, ~A failed ===~%" *passed* *failed*)
