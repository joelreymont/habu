#!/usr/bin/env sbcl --script
;;; Test defmacro and macro expansion via run-bytecode runtime.

(load "run-habu.lisp")

(defun run-test (name forms expected)
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (format t "~A: pass~%" name)
        (progn
          (format t "~A: FAIL (expected ~A, got ~A)~%" name expected result)
          (sb-ext:quit :unix-status 1)))))

;; Test 1: Simple macro that doubles a value
(run-test "macro-double"
          '((defmacro double (x) `(+ ,x ,x))
            (double #x5))
          #xA)

;; Test 2: Macro with multiple args
(run-test "macro-add3"
          '((defmacro add3 (a b c) `(+ ,a (+ ,b ,c)))
            (add3 #x1 #x2 #x3))
          #x6)

;; Test 3: Macro used in function body
(run-test "macro-in-defun"
          '((defmacro square (x) `(* ,x ,x))
            (defun sq (n) (square n))
            (sq #x4))
          #x10)

;; Test 4: Nested macro calls
(run-test "macro-nested"
          '((defmacro inc (x) `(+ ,x #x1))
            (defmacro inc2 (x) `(inc (inc ,x)))
            (inc2 #x3))
          #x5)

(format t "All macro tests passed~%")
(sb-ext:quit :unix-status 0)
