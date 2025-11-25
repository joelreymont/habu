#!/usr/bin/env sbcl --script
;;; Test recursive list functions via run-bytecode runtime.

(load "run-habu.lisp")

(defun run-test (name forms expected)
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (format t "~A: pass~%" name)
        (progn
          (format t "~A: FAIL (expected ~A, got ~A)~%" name expected result)
          (sb-ext:quit :unix-status 1)))))

;; Length tests
(run-test "length-nil"
          '((length #x0))
          #x0)

(run-test "length-one"
          '((length (list #x1)))
          #x1)

(run-test "length-three"
          '((length (list #x1 #x2 #x3)))
          #x3)

(run-test "length-five"
          '((length (list #x1 #x2 #x3 #x4 #x5)))
          #x5)

;; Append tests
(run-test "append-nil-nil"
          '((null (append #x0 #x0)))
          #x1)

(run-test "append-nil-list"
          '((car (append #x0 (list #x1 #x2))))
          #x1)

(run-test "append-list-nil"
          '((car (append (list #x1 #x2) #x0)))
          #x1)

(run-test "append-two-lists"
          '((length (append (list #x1 #x2) (list #x3 #x4))))
          #x4)

(run-test "append-check-order"
          '((car (cdr (cdr (append (list #x1 #x2) (list #x3 #x4))))))
          #x3)

;; Reverse tests
(run-test "reverse-nil"
          '((null (reverse #x0)))
          #x1)

(run-test "reverse-one"
          '((car (reverse (list #x42))))
          #x42)

(run-test "reverse-three-first"
          '((car (reverse (list #x1 #x2 #x3))))
          #x3)

(run-test "reverse-three-last"
          '((car (cdr (cdr (reverse (list #x1 #x2 #x3))))))
          #x1)

;; Assoc tests
(run-test "assoc-found-first"
          '((cdr (assoc #x1 (acons #x1 #x10 (acons #x2 #x20 #x0)))))
          #x10)

(run-test "assoc-found-second"
          '((cdr (assoc #x2 (acons #x1 #x10 (acons #x2 #x20 #x0)))))
          #x20)

(run-test "assoc-not-found"
          '((null (assoc #x3 (acons #x1 #x10 (acons #x2 #x20 #x0)))))
          #x1)

;; Member tests
(run-test "member-found-first"
          '((null (member #x1 (list #x1 #x2 #x3))))
          #x0)  ; not nil, so null returns 0

(run-test "member-found-check"
          '((car (member #x2 (list #x1 #x2 #x3))))
          #x2)

(run-test "member-not-found"
          '((null (member #x4 (list #x1 #x2 #x3))))
          #x1)

(format t "All recursive list function tests passed~%")
(sb-ext:quit :unix-status 0)
