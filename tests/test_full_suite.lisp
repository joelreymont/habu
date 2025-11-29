(load "bootstrap/compiler.lisp")
(load "bootstrap/macho.lisp")
(in-package :habu)

(format t "~%=== Full Compiler Test Suite ===~%~%")

(defvar *pass* 0)
(defvar *fail* 0)

(defun test (name src exp)
  (handler-case
    (progn
      (deliver-with-libsystem src (format nil "/tmp/ts_~A" name))
      (let ((res (sb-ext:process-exit-code 
                  (sb-ext:run-program (format nil "/tmp/ts_~A" name) nil 
                                      :output nil :error nil :wait t))))
        (if (= res exp)
            (progn (format t "[PASS] ~A~%" name) (incf *pass*))
            (progn (format t "[FAIL] ~A: expected ~A, got ~A~%" name exp res) (incf *fail*)))))
    (error (e) (format t "[ERROR] ~A: ~A~%" name e) (incf *fail*))))

;; === Arithmetic ===
(test "add" "(sys-exit (+ 20 22))" 42)
(test "sub" "(sys-exit (- 100 58))" 42)
(test "mul" "(sys-exit (* 6 7))" 42)
(test "div" "(sys-exit (/ 84 2))" 42)
(test "mod" "(sys-exit (mod 47 5))" 2)
(test "nested" "(sys-exit (+ (* 3 4) (+ 5 7)))" 24)

;; === Comparisons ===
(test "eq-t" "(sys-exit (if (= 5 5) 42 0))" 42)
(test "eq-f" "(sys-exit (if (= 5 6) 0 42))" 42)
(test "lt" "(sys-exit (if (< 3 5) 42 0))" 42)
(test "gt" "(sys-exit (if (> 7 5) 42 0))" 42)
(test "le" "(sys-exit (if (<= 5 5) 42 0))" 42)
(test "ge" "(sys-exit (if (>= 5 5) 42 0))" 42)

;; === Let bindings ===
(test "let" "(sys-exit (let ((x 42)) x))" 42)
(test "let-star" "(sys-exit (let* ((x 6) (y (* x 7))) y))" 42)
(test "let-nest" "(sys-exit (let ((x 10)) (let ((y 32)) (+ x y))))" 42)
(test "let-multi" "(sys-exit (let ((a 10) (b 20) (c 12)) (+ a (+ b c))))" 42)

;; === Cons cells ===
(test "car" "(sys-exit (car (cons 42 0)))" 42)
(test "cdr" "(sys-exit (cdr (cons 0 42)))" 42)
(test "cadr" "(sys-exit (car (cdr (cons 1 (cons 42 nil)))))" 42)
(test "list" "(sys-exit (car (cdr (cdr (quote (1 2 42))))))" 42)

;; === List functions ===
(test "length" "(sys-exit (length (quote (1 2 3))))" 3)
(test "reverse" "(sys-exit (car (reverse (quote (1 2 3)))))" 3)
(test "append" "(sys-exit (length (append (quote (1 2)) (quote (3 4 5)))))" 5)

;; === Predicates ===
(test "null-t" "(sys-exit (if (null nil) 42 0))" 42)
(test "null-f" "(sys-exit (if (null (cons 1 2)) 0 42))" 42)
(test "consp-t" "(sys-exit (if (consp (cons 1 2)) 42 0))" 42)
(test "consp-f" "(sys-exit (if (consp 5) 0 42))" 42)
(test "numberp" "(sys-exit (if (numberp 5) 42 0))" 42)
(test "symbolp" "(sys-exit (if (symbolp (quote x)) 42 0))" 42)

;; === Defun and recursion ===
(test "defun" "(defun f (x) (+ x 2)) (sys-exit (f 40))" 42)
(test "fact" "(defun fact (n acc) (if (= n 0) acc (fact (- n 1) (* n acc)))) (sys-exit (fact 5 1))" 120)
(test "fib" "(defun fib (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (sys-exit (fib 10))" 55)

;; === Lambda and funcall ===
(test "funcall-inline" "(sys-exit (funcall (lambda (x) (+ x 2)) 40))" 42)
(test "funcall-let" "(sys-exit (let ((f (lambda (x) (+ x 2)))) (funcall f 40)))" 42)
(test "funcall-nested" "(sys-exit (funcall (lambda (x) (funcall (lambda (y) (+ y 2)) x)) 40))" 42)

;; === Closures ===
(test "closure" "(defun make-adder (n) (lambda (x) (+ x n))) (sys-exit (funcall (make-adder 10) 32))" 42)
(test "closure-multi" "(defun make-fn (a b) (lambda (x) (+ a (+ b x)))) (sys-exit (funcall (make-fn 10 20) 12))" 42)
(test "closure-nested" "(defun outer (x) (lambda (y) (lambda (z) (+ x (+ y z))))) (let* ((f1 (outer 10)) (f2 (funcall f1 20))) (sys-exit (funcall f2 12)))" 42)

;; === Mutable closures ===
(test "counter" "(defun make-counter () (let ((n 0)) (lambda () (setq n (+ n 1)) n))) (let ((c (make-counter))) (funcall c) (funcall c) (sys-exit (funcall c)))" 3)
(test "counter-2" "(defun make-counter (start) (let ((n start)) (lambda () (setq n (+ n 1)) n))) (let ((c1 (make-counter 0)) (c2 (make-counter 100))) (funcall c1) (funcall c2) (sys-exit (+ (funcall c1) (funcall c2))))" 104)

;; === Labels ===
(test "labels" "(sys-exit (labels ((fact (n acc) (if (= n 0) acc (fact (- n 1) (* n acc))))) (fact 5 1)))" 120)
(test "labels-mutual" "(sys-exit (labels ((even? (n) (if (= n 0) 1 (odd? (- n 1)))) (odd? (n) (if (= n 0) 0 (even? (- n 1))))) (even? 10)))" 1)

;; === Vectors ===
(test "vector" "(sys-exit (let ((v (make-vector 3))) (vector-set v 0 42) (vector-ref v 0)))" 42)
(test "vec-len" "(sys-exit (vector-length (make-vector 5)))" 5)

;; === Strings ===
(test "str-len" "(sys-exit (string-length \"hello\"))" 5)
(test "str-ref" "(sys-exit (string-ref \"ABC\" 0))" 65)

;; === Bitwise ===
(test "logand" "(sys-exit (logand 255 42))" 42)
(test "logior" "(sys-exit (logior 32 10))" 42)
(test "ash-l" "(sys-exit (ash 21 1))" 42)
(test "ash-r" "(sys-exit (ash 84 -1))" 42)

;; === Setq ===
(test "setq" "(sys-exit (let ((x 10)) (setq x 42) x))" 42)
(test "setcar" "(sys-exit (let ((c (cons 0 0))) (setcar c 42) (car c)))" 42)
(test "setcdr" "(sys-exit (let ((c (cons 0 0))) (setcdr c 42) (cdr c)))" 42)

(format t "~%=== Results: ~A passed, ~A failed ===~%" *pass* *fail*)
