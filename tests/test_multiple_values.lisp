#!/usr/bin/env sbcl --script
;;; Test multiple values via run-bytecode runtime.

(load "run-habu.lisp")

(defun run-test (name forms expected)
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (format t "~A: pass~%" name)
        (progn
          (format t "~A: FAIL (expected ~A, got ~A)~%" name expected result)
          (sb-ext:quit :unix-status 1)))))

;; Test 1: Single value returns that value
(run-test "values-single"
          '((values #x5))
          #x5)

;; Test 2: No values returns nil
(run-test "values-zero"
          '((values))
          #x0)

;; Test 3: Multiple values returns first
(run-test "values-primary"
          '((values #xA #xB #xC))
          #xA)

;; Test 4: multiple-value-bind with single value
(run-test "mvb-single"
          '((multiple-value-bind (x) (values #x7)
              x))
          #x7)

;; Test 5: multiple-value-bind with two values
(run-test "mvb-two"
          '((multiple-value-bind (x y) (values #x3 #x4)
              (+ x y)))
          #x7)

;; Test 6: multiple-value-bind with three values
(run-test "mvb-three"
          '((multiple-value-bind (a b c) (values #x1 #x2 #x3)
              (+ a (+ b c))))
          #x6)

;; Test 7: values with expression arguments
(run-test "values-expr"
          '((multiple-value-bind (x y) (values (+ #x1 #x2) (* #x3 #x4))
              (+ x y)))
          #xF)

;; Test 8: Function returning multiple values
(run-test "mvb-defun"
          '((defun divmod (n d)
              (values (/ n d) (mod n d)))
            (multiple-value-bind (q r) (divmod #xA #x3)
              (+ (* q #x3) r)))
          #xA)

(format t "All multiple values tests passed~%")
(sb-ext:quit :unix-status 0)
