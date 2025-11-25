#!/usr/bin/env sbcl --script
;;; Test type predicates via run-bytecode runtime.
;;; Note: results are untagged by parse-run-bytecode-output, so true=1, false=0

(load "run-habu.lisp")

(defun run-test (name forms expected)
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (format t "~A: pass~%" name)
        (progn
          (format t "~A: FAIL (expected ~A, got ~A)~%" name expected result)
          (sb-ext:quit :unix-status 1)))))

;; null tests (untagged: true=1, false=0)
(run-test "null-nil"
          '((null #x0))
          #x1) ; t untagged

(run-test "null-non-nil"
          '((null #x5))
          #x0) ; nil

;; numberp tests (fixnum tag = 0)
(run-test "numberp-fixnum"
          '((numberp #x42))
          #x1) ; t untagged

;; consp tests (cons tag = 1)
(run-test "consp-cons"
          '((consp (cons #x1 #x2)))
          #x1) ; t untagged

(run-test "consp-fixnum"
          '((consp #x5))
          #x0) ; nil

;; atom tests (not cons)
(run-test "atom-fixnum"
          '((atom #x5))
          #x1) ; t untagged

(run-test "atom-cons"
          '((atom (cons #x1 #x2)))
          #x0) ; nil

(run-test "atom-nil"
          '((atom #x0))
          #x1) ; t untagged (nil is atom)

;; zerop tests
(run-test "zerop-zero"
          '((zerop #x0))
          #x1) ; t untagged

(run-test "zerop-nonzero"
          '((zerop #x5))
          #x0)

;; plusp tests
(run-test "plusp-positive"
          '((plusp #x5))
          #x1) ; t untagged

(run-test "plusp-zero"
          '((plusp #x0))
          #x0)

(format t "All type predicate tests passed~%")
(sb-ext:quit :unix-status 0)
