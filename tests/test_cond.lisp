#!/usr/bin/env sbcl --script
;;; Test cond multi-way conditional via run-bytecode runtime.

(load "run-habu.lisp")

(defun run-test (name forms expected)
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (format t "~A: pass~%" name)
        (progn
          (format t "~A: FAIL (expected ~A, got ~A)~%" name expected result)
          (sb-ext:quit :unix-status 1)))))

;; Test 1: cond first clause matches
(run-test "cond-first-match"
          '((defun classify (x)
              (cond ((< x #x10) #x1)
                    ((= x #x10) #x2)
                    (t #x3)))
            (classify #x5))
          #x1)

;; Test 2: cond second clause match
(run-test "cond-second-match"
          '((defun classify (x)
              (cond ((< x #x10) #x1)
                    ((= x #x10) #x2)
                    (t #x3)))
            (classify #x10))
          #x2)

;; Test 3: cond default clause
(run-test "cond-default"
          '((defun classify (x)
              (cond ((< x #x10) #x1)
                    ((= x #x10) #x2)
                    (t #x3)))
            (classify #x20))
          #x3)

;; Test 4: cond with no matching clause returns nil (0)
(run-test "cond-no-match"
          '((cond ((= #x1 #x2) #x99)))
          #x0)

;; Test 5: cond with progn body
(run-test "cond-progn-body"
          '((defun multi-body (x)
              (cond ((> x #x0)
                     (+ x #x1)
                     (+ x #x2))))
            (multi-body #x3))
          #x5)

(format t "All cond tests passed~%")
(sb-ext:quit :unix-status 0)
