;;; Native Mach-O executable tests for the bootstrap compiler
;;; Tests inline heap allocation, symbols, closures without runtime
(load "bootstrap/compiler.lisp")
(load "bootstrap/macho.lisp")

(defpackage :habu-test-native-macho
  (:use :cl))

(in-package :habu-test-native-macho)

(format t "~%=== Native Mach-O Executable Tests ===~%~%")

(defvar *pass-count* 0)
(defvar *fail-count* 0)

(defun test-native (name source expected)
  "Test native delivery. SOURCE is an expression; we wrap it in (sys-exit ...)."
  (handler-case
    (let ((output-path (format nil "/tmp/native_~A" name))
          (full-source (format nil "(sys-exit ~A)" source)))
      (deliver full-source output-path :verbose nil)
      (let* ((proc (sb-ext:run-program output-path nil :output nil :error nil :wait t))
             (result (sb-ext:process-exit-code proc)))
        (if (= result expected)
            (progn (format t "[PASS] ~A = ~A~%" name result)
                   (incf *pass-count*))
            (progn (format t "[FAIL] ~A: expected ~A, got ~A~%" name expected result)
                   (incf *fail-count*)))))
    (error (e)
      (format t "[FAIL] ~A: error ~A~%" name e)
      (incf *fail-count*))))

(defun test-native-full (name source expected)
  "Test native delivery with full source (already contains sys-exit or defuns)."
  (handler-case
    (let ((output-path (format nil "/tmp/native_~A" name)))
      (deliver source output-path :verbose nil)
      (let* ((proc (sb-ext:run-program output-path nil :output nil :error nil :wait t))
             (result (sb-ext:process-exit-code proc)))
        (if (= result expected)
            (progn (format t "[PASS] ~A = ~A~%" name result)
                   (incf *pass-count*))
            (progn (format t "[FAIL] ~A: expected ~A, got ~A~%" name expected result)
                   (incf *fail-count*)))))
    (error (e)
      (format t "[FAIL] ~A: error ~A~%" name e)
      (incf *fail-count*))))

;;; Basic arithmetic
(test-native "add" "(+ 20 22)" 42)
(test-native "sub" "(- 100 58)" 42)
(test-native "mul" "(* 6 7)" 42)
(test-native "div" "(/ 84 2)" 42)
(test-native "nested-arith" "(+ (* 3 4) (+ 5 7))" 24)
(test-native "complex-arith" "(+ (* 2 3) (- 40 4))" 42)

;;; Comparisons
(test-native "cmp-eq-true" "(if (= 5 5) 42 0)" 42)
(test-native "cmp-eq-false" "(if (= 5 6) 0 42)" 42)
(test-native "cmp-lt" "(if (< 3 5) 42 0)" 42)
(test-native "cmp-gt" "(if (> 7 5) 42 0)" 42)
(test-native "cmp-le" "(if (<= 5 5) 42 0)" 42)
(test-native "cmp-ge" "(if (>= 5 5) 42 0)" 42)

;;; Let bindings
(test-native "let-simple" "(let ((x 42)) x)" 42)
(test-native "let-nested" "(let ((x 10)) (let ((y 32)) (+ x y)))" 42)
(test-native "let-star" "(let* ((x 6) (y (* x 7))) y)" 42)
(test-native "let-multi" "(let ((a 10) (b 20) (c 12)) (+ a (+ b c)))" 42)

;;; Cons cells (inline heap allocation)
(test-native "car-cons" "(car (cons 42 0))" 42)
(test-native "cdr-cons" "(cdr (cons 0 42))" 42)
(test-native "cadr-cons" "(car (cdr (cons 1 (cons 42 nil))))" 42)
(test-native "nested-cons" "(car (car (cons (cons 42 0) 0)))" 42)
(test-native "list-len3" "(car (cdr (cdr (cons 1 (cons 2 (cons 42 nil))))))" 42)

;;; Type predicates
(test-native "consp-true" "(if (consp (cons 1 2)) 42 0)" 42)
(test-native "consp-false" "(if (consp 5) 0 42)" 42)
(test-native "null-true" "(if (null nil) 42 0)" 42)
(test-native "null-false" "(if (null (cons 1 2)) 0 42)" 42)

;;; Symbols (inline compile-time symbol table)
(test-native "if-t" "(if t 42 0)" 42)
(test-native "eq-sym-same" "(if (eq (quote foo) (quote foo)) 42 0)" 42)
(test-native "eq-sym-diff" "(if (eq (quote foo) (quote bar)) 0 42)" 42)
(test-native "eq-num" "(if (eq 5 5) 42 0)" 42)

;;; Simple functions
(test-native-full "defun-id" "(defun id (x) x) (sys-exit (id 42))" 42)
(test-native-full "defun-add1" "(defun add1 (x) (+ x 1)) (sys-exit (add1 41))" 42)
(test-native-full "defun-double" "(defun double (x) (* x 2)) (sys-exit (double 21))" 42)
(test-native-full "defun-with-let" "(defun f (x) (let ((y 10)) (+ x y))) (sys-exit (f 32))" 42)

;;; Multiple functions
(test-native-full "two-defuns"
  "(defun f (x) (+ x 1)) (defun g (x) (* x 2)) (sys-exit (g (f 20)))" 42)
(test-native-full "call-chain"
  "(defun a (x) (+ x 1)) (defun b (x) (a (a x))) (sys-exit (b 40))" 42)

;;; Recursive functions
(test-native-full "fact5"
  "(defun fact (n) (if (= n 0) 1 (* n (fact (- n 1))))) (sys-exit (fact 5))" 120)
(test-native-full "sum-to-10"
  "(defun sum-to (n) (if (= n 0) 0 (+ n (sum-to (- n 1))))) (sys-exit (sum-to 10))" 55)
(test-native-full "fib8"
  "(defun fib (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (sys-exit (fib 8))" 21)

;;; Recursive list processing (combines cons and recursion)
(test-native-full "sum-list"
  "(defun sum-list (l) (if (null l) 0 (+ (car l) (sum-list (cdr l)))))
   (sys-exit (sum-list (cons 10 (cons 20 (cons 12 nil)))))" 42)
(test-native-full "len-list"
  "(defun len (l) (if (null l) 0 (+ 1 (len (cdr l)))))
   (sys-exit (len (cons 1 (cons 2 (cons 3 (cons 4 nil))))))" 4)

;;; Labels (inline closures)
(test-native "labels-fact"
  "(labels ((fact (n a) (if (= n 0) a (fact (- n 1) (* n a)))))
     (fact 5 1))" 120)
(test-native "labels-sum"
  "(labels ((sum (n a) (if (= n 0) a (sum (- n 1) (+ a n)))))
     (sum 10 0))" 55)
(test-native "labels-fib"
  "(labels ((fib (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))
     (fib 10))" 55)

;;; Mutual recursion with labels
(test-native "even-odd-10"
  "(labels ((is-even (n) (if (= n 0) 1 (is-odd (- n 1))))
            (is-odd (n) (if (= n 0) 0 (is-even (- n 1)))))
     (is-even 10))" 1)
(test-native "even-odd-7"
  "(labels ((is-even (n) (if (= n 0) 1 (is-odd (- n 1))))
            (is-odd (n) (if (= n 0) 0 (is-even (- n 1)))))
     (is-even 7))" 0)

;;; Multiple values and complex control flow
(test-native "cond-first"
  "(cond ((= 1 1) 42) ((= 2 2) 0) (t 0))" 42)
(test-native "cond-second"
  "(cond ((= 1 2) 0) ((= 2 2) 42) (t 0))" 42)
(test-native "cond-default"
  "(cond ((= 1 2) 0) ((= 2 3) 0) (t 42))" 42)

;;; Progn
(test-native "progn-simple" "(progn 1 2 42)" 42)
(test-native "progn-with-let" "(let ((x 0)) (progn (+ x 1) (+ x 40) 42))" 42)

;;; Vectors (inline heap allocation)
(test-native "make-vector"
  "(let ((v (make-vector 3))) (if (> v 0) 42 0))" 42)
(test-native "vector-set-ref"
  "(let ((v (make-vector 3)))
     (vector-set v 0 42)
     (vector-ref v 0))" 42)
(test-native "vector-set-multiple"
  "(let ((v (make-vector 3)))
     (vector-set v 0 10)
     (vector-set v 1 20)
     (vector-set v 2 12)
     (+ (vector-ref v 0) (+ (vector-ref v 1) (vector-ref v 2))))" 42)

;;; Strings (inline string access)
(test-native "string-length"
  "(string-length \"hello\")" 5)
(test-native "string-ref-0"
  "(string-ref \"ABC\" 0)" 65)      ; 'A'
(test-native "string-ref-1"
  "(string-ref \"ABC\" 1)" 66)      ; 'B'

;;; List functions (for self-hosting)
(test-native "length-nil" "(length nil)" 0)
(test-native "length-3" "(length (cons 1 (cons 2 (cons 3 nil))))" 3)
(test-native "reverse-nil" "(if (null (reverse nil)) 42 0)" 42)
(test-native "reverse-list"
  "(car (reverse (cons 1 (cons 2 (cons 3 nil)))))" 3)
(test-native "append-nil" "(if (null (append nil nil)) 42 0)" 42)
(test-native "append-lists"
  "(car (cdr (append (cons 1 nil) (cons 2 (cons 3 nil)))))" 2)
(test-native "member-found"
  "(if (member 2 (cons 1 (cons 2 (cons 3 nil)))) 42 0)" 42)
(test-native "member-not-found"
  "(if (member 9 (cons 1 (cons 2 nil))) 0 42)" 42)
(test-native "assoc-found"
  "(cdr (assoc 2 (cons (cons 1 10) (cons (cons 2 42) nil))))" 42)
(test-native "assoc-not-found"
  "(if (assoc 9 (cons (cons 1 10) nil)) 0 42)" 42)

;;; List accessors
(test-native "cadr"
  "(cadr (cons 1 (cons 42 nil)))" 42)
(test-native "caddr"
  "(caddr (cons 1 (cons 2 (cons 42 nil))))" 42)
(test-native "cadddr"
  "(cadddr (cons 1 (cons 2 (cons 3 (cons 42 nil)))))" 42)
(test-native "cddr"
  "(car (cddr (cons 1 (cons 2 (cons 42 nil)))))" 42)
(test-native "cdddr"
  "(car (cdddr (cons 1 (cons 2 (cons 3 (cons 42 nil))))))" 42)
(test-native "first-second"
  "(+ (first (cons 10 nil)) (second (cons 0 (cons 32 nil))))" 42)
(test-native "third-fourth"
  "(+ (third (cons 1 (cons 2 (cons 20 nil))))
      (fourth (cons 1 (cons 2 (cons 3 (cons 22 nil))))))" 42)
(test-native "rest"
  "(car (rest (cons 1 (cons 42 nil))))" 42)
(test-native "nth-0"
  "(nth 0 (cons 42 (cons 2 nil)))" 42)
(test-native "nth-2"
  "(nth 2 (cons 1 (cons 2 (cons 42 nil))))" 42)
(test-native "count-eq"
  "(count 2 (cons 1 (cons 2 (cons 2 (cons 3 nil)))))" 2)

(format t "~%=== Results: ~A passed, ~A failed ===~%"
        *pass-count* *fail-count*)
(sb-ext:exit :code *fail-count*)
