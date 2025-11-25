#!/usr/bin/env sbcl --script
;;; Tests for gensym and intern functions

(load "run-habu.lisp")

(defun run-test (name forms expected)
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (format t "~A: pass~%" name)
        (progn
          (format t "~A: FAIL (expected ~X, got ~X)~%" name expected result)
          (sb-ext:quit :unix-status 1)))))

(format t "~%=== Gensym/Intern Tests ===~%~%")

;; Test 1: gensym produces a symbol
(run-test "gensym-is-symbol"
          '((symbolp (gensym)))
          #x1)

;; Test 2: gensym produces different symbols each time
(run-test "gensym-unique"
          '((let ((s1 (gensym))
                  (s2 (gensym)))
              (if (eq s1 s2) #x0 #x1)))
          #x1)

;; Test 3: intern creates a symbol
(run-test "intern-is-symbol"
          '((symbolp (intern "TEST")))
          #x1)

;; Test 4: intern with same name returns same symbol
(run-test "intern-same-name"
          '((eq (intern "SAME") (intern "SAME")))
          #x1)

;; Test 5: gensym with prefix produces a symbol
(run-test "gensym-with-prefix"
          '((symbolp (gensym "PREFIX-")))
          #x1)

(format t "~%=== All Gensym/Intern Tests Passed ===~%")
(sb-ext:quit :unix-status 0)
