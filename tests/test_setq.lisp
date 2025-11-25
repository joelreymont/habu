#!/usr/bin/env sbcl --script
;;; Test setq, setf, incf, decf, push, let* via run-bytecode runtime.

(load "run-habu.lisp")

(defun run-test (name forms expected)
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (format t "~A: pass~%" name)
        (progn
          (format t "~A: FAIL (expected ~A, got ~A)~%" name expected result)
          (sb-ext:quit :unix-status 1)))))

;; Setq tests
(run-test "setq-basic"
          '((let ((x #x5))
              (setq x #x10)
              x))
          #x10)

(run-test "setq-returns-value"
          '((let ((x #x0))
              (setq x #x42)))
          #x42)

(run-test "setq-multiple"
          '((let ((x #x1))
              (progn
                (setq x #x2)
                (setq x #x3)
                x)))
          #x3)

;; Let* tests (sequential binding)
(run-test "let*-sequential"
          '((let* ((a #x5)
                   (b (+ a #x1)))
              b))
          #x6)

(run-test "let*-chain"
          '((let* ((x #x1)
                   (y (+ x #x1))
                   (z (+ y #x1)))
              z))
          #x3)

;; Incf tests
(run-test "incf-basic"
          '((let ((x #x5))
              (progn (incf x) x)))
          #x6)

(run-test "incf-delta"
          '((let ((x #x5))
              (progn (incf x #x10) x)))
          #x15)

;; Decf tests
(run-test "decf-basic"
          '((let ((x #x10))
              (progn (decf x) x)))
          #xF)

(run-test "decf-delta"
          '((let ((x #x10))
              (progn (decf x #x3) x)))
          #xD)

;; Setf on variables (same as setq)
(run-test "setf-var"
          '((let ((x #x5))
              (progn (setf x #x20) x)))
          #x20)

;; Push tests
(run-test "push-basic"
          '((let ((lst #x0))
              (progn (push #x1 lst) (car lst))))
          #x1)

(run-test "push-multiple"
          '((let ((lst #x0))
              (progn
                (push #x1 lst)
                (push #x2 lst)
                (car lst))))
          #x2)

(format t "All setq/mutation tests passed~%")
(sb-ext:quit :unix-status 0)
