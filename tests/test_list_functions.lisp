#!/usr/bin/env sbcl --script
;;; Test list functions via run-bytecode runtime.

(load "run-habu.lisp")

(defun run-test (name forms expected)
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (format t "~A: pass~%" name)
        (progn
          (format t "~A: FAIL (expected ~A, got ~A)~%" name expected result)
          (sb-ext:quit :unix-status 1)))))

;; Acons tests
(run-test "acons-key"
          '((car (car (acons #x1 #x2 #x0))))
          #x1)

(run-test "acons-val"
          '((cdr (car (acons #x1 #x2 #x0))))
          #x2)

(run-test "acons-tail"
          '((null (cdr (acons #x1 #x2 #x0))))
          #x1)

(run-test "acons-existing"
          '((car (car (acons #x3 #x4 (acons #x1 #x2 #x0)))))
          #x3)

;; Nth tests (compile-time unrolled)
(run-test "nth-0"
          '((nth 0 (list #x10 #x20 #x30)))
          #x10)

(run-test "nth-1"
          '((nth 1 (list #x10 #x20 #x30)))
          #x20)

(run-test "nth-2"
          '((nth 2 (list #x10 #x20 #x30)))
          #x30)

;; Nthcdr tests
(run-test "nthcdr-0"
          '((car (nthcdr 0 (list #x10 #x20 #x30))))
          #x10)

(run-test "nthcdr-1"
          '((car (nthcdr 1 (list #x10 #x20 #x30))))
          #x20)

(run-test "nthcdr-2"
          '((car (nthcdr 2 (list #x10 #x20 #x30))))
          #x30)

;; Elt tests
(run-test "elt-0"
          '((elt (list #x10 #x20 #x30) 0))
          #x10)

(run-test "elt-1"
          '((elt (list #x10 #x20 #x30) 1))
          #x20)

;; Identity test
(run-test "identity"
          '((identity #x42))
          #x42)

(format t "All list function tests passed~%")
(sb-ext:quit :unix-status 0)
