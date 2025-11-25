#!/usr/bin/env sbcl --script
;;; Test labels and flet (local functions) via run-bytecode runtime.

(load "run-habu.lisp")

(defun run-test (name forms expected)
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (format t "~A: pass~%" name)
        (progn
          (format t "~A: FAIL (expected ~A, got ~A)~%" name expected result)
          (sb-ext:quit :unix-status 1)))))

;; Basic flet (non-recursive)
(run-test "flet-basic"
          '((flet ((add1 (x) (+ x #x1)))
              (add1 #x5)))
          #x6)

(run-test "flet-two-funcs"
          '((flet ((add1 (x) (+ x #x1))
                   (double (x) (+ x x)))
              (+ (add1 #x5) (double #x3))))
          #xC)

;; Basic labels (recursive)
(run-test "labels-simple"
          '((labels ((f (x) (+ x #x1)))
              (f #x5)))
          #x6)

;; Recursive factorial
(run-test "labels-factorial"
          '((labels ((fact (n)
                       (if (= n #x0)
                           #x1
                           (* n (fact (- n #x1))))))
              (fact #x5)))
          #x78)  ; 5! = 120 = 0x78

;; Recursive sum
(run-test "labels-sum"
          '((labels ((sum (n)
                       (if (= n #x0)
                           #x0
                           (+ n (sum (- n #x1))))))
              (sum #x5)))
          #xF)   ; 1+2+3+4+5 = 15 = 0xF

;; Mutual recursion
(run-test "labels-mutual-even"
          '((labels ((is-even (n)
                       (if (= n #x0)
                           #x1
                           (is-odd (- n #x1))))
                     (is-odd (n)
                       (if (= n #x0)
                           #x0
                           (is-even (- n #x1)))))
              (is-even #x4)))
          #x1)

(run-test "labels-mutual-odd"
          '((labels ((is-even (n)
                       (if (= n #x0)
                           #x1
                           (is-odd (- n #x1))))
                     (is-odd (n)
                       (if (= n #x0)
                           #x0
                           (is-even (- n #x1)))))
              (is-odd #x5)))
          #x1)

;; Nested labels
(run-test "labels-nested"
          '((labels ((outer (x)
                       (labels ((inner (y)
                                  (+ y #x1)))
                         (inner (+ x #x10)))))
              (outer #x5)))
          #x16)

(format t "All labels/flet tests passed~%")
(sb-ext:quit :unix-status 0)
