#!/usr/bin/env sbcl --script
;;; Test utility functions via run-bytecode runtime.
;;; Results are untagged fixnums; true=1, false=0

(load "run-habu.lisp")

(defun run-test (name forms expected)
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (format t "~A: pass~%" name)
        (progn
          (format t "~A: FAIL (expected ~A, got ~A)~%" name expected result)
          (sb-ext:quit :unix-status 1)))))

;; Eq tests
(run-test "eq-same" '((eq #x5 #x5)) #x1)
(run-test "eq-diff" '((eq #x5 #x6)) #x0)

;; Eql tests
(run-test "eql-same" '((eql #x5 #x5)) #x1)
(run-test "eql-diff" '((eql #x5 #x6)) #x0)

;; 1+ tests
(run-test "1+-positive" '((1+ #x5)) #x6)
(run-test "1+-zero" '((1+ #x0)) #x1)

;; 1- tests
(run-test "1--positive" '((1- #x5)) #x4)
(run-test "1--one" '((1- #x1)) #x0)

;; Max tests
(run-test "max-first" '((max #x10 #x5)) #x10)
(run-test "max-second" '((max #x5 #x10)) #x10)
(run-test "max-equal" '((max #x7 #x7)) #x7)

;; Min tests
(run-test "min-first" '((min #x5 #x10)) #x5)
(run-test "min-second" '((min #x10 #x5)) #x5)
(run-test "min-equal" '((min #x7 #x7)) #x7)

;; List accessor tests
(run-test "cadr"
          '((cadr (cons #x1 (cons #x2 (cons #x3 #x0)))))
          #x2)

(run-test "caddr"
          '((caddr (cons #x1 (cons #x2 (cons #x3 #x0)))))
          #x3)

(run-test "cddr"
          '((car (cddr (cons #x1 (cons #x2 (cons #x3 #x0))))))
          #x3)

(run-test "first"
          '((first (cons #x1 (cons #x2 #x0))))
          #x1)

(run-test "second"
          '((second (cons #x1 (cons #x2 #x0))))
          #x2)

(run-test "third"
          '((third (cons #x1 (cons #x2 (cons #x3 #x0)))))
          #x3)

(run-test "rest"
          '((car (rest (cons #x1 (cons #x2 #x0)))))
          #x2)

;; List tests
(run-test "list-empty" '((null (list))) #x1)

(run-test "list-one"
          '((car (list #x42)))
          #x42)

(run-test "list-two-first"
          '((car (list #x1 #x2)))
          #x1)

(run-test "list-two-second"
          '((cadr (list #x1 #x2)))
          #x2)

(run-test "list-three"
          '((caddr (list #x1 #x2 #x3)))
          #x3)

;; List* tests
(run-test "list*-one"
          '((list* #x5))
          #x5)

(run-test "list*-two"
          '((car (list* #x1 (cons #x2 #x0))))
          #x1)

(run-test "list*-three"
          '((cadr (list* #x1 #x2 #x0)))
          #x2)

(format t "All utility function tests passed~%")
(sb-ext:quit :unix-status 0)
