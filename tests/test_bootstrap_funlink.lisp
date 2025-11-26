;;; Test function linking for multi-function programs
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
      (with-open-file (out "/tmp/fn-test.bin" :direction :output
                           :if-exists :supersede
                           :element-type '(unsigned-byte 8))
        (dolist (byte code) (write-byte byte out)))
      (let* ((output (with-output-to-string (s)
                       (sb-ext:run-program "./run-bytecode" '("/tmp/fn-test.bin")
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

(format t "~%=== Bootstrap Function Linking Tests ===~%~%")

;; Basic function calls
(test-case "simple-fn" "(defun f (x) (+ x 1)) (f 10)" 11)
(test-case "two-arg-fn" "(defun add (a b) (+ a b)) (add 10 20)" 30)
(test-case "three-arg-fn" "(defun sum3 (a b c) (+ a (+ b c))) (sum3 1 2 3)" 6)

;; Multiple calls to same function
(test-case "nested-call" "(defun f (x) (+ x 1)) (+ (f 5) (f 10))" 17)
(test-case "triple-call" "(defun f (x) (* x 2)) (+ (f 1) (+ (f 2) (f 3)))" 12)

;; Multiple functions
(test-case "two-fns" "(defun f (x) (+ x 1)) (defun g (x) (* x 2)) (+ (f 3) (g 5))" 14)
(test-case "fn-calls-fn" "(defun f (x) (+ x 1)) (defun g (x) (f (f x))) (g 5)" 7)

;; Functions with let bindings
(test-case "fn-with-let" "(defun f (x) (let ((y 5)) (+ x y))) (f 10)" 15)
(test-case "let-around-call" "(defun f (x) (+ x 1)) (let ((a (f 5))) (+ a 10))" 16)
(test-case "let-seq-calls" "(defun f (x) (+ x 1)) (let ((a (f 5))) (let ((b (f 10))) (+ a b)))" 17)

;; Recursive functions
(test-case "factorial" "(defun fact (n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact 5)" 120)
(test-case "fibonacci" "(defun fib (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 10)" 55)

;; Mutual recursion
(test-case "even-odd"
           "(defun even? (n) (if (= n 0) 1 (odd? (- n 1)))) (defun odd? (n) (if (= n 0) 0 (even? (- n 1)))) (even? 4)"
           1)

(format t "~%=== Results: ~A passed, ~A failed ===~%" *passed* *failed*)
