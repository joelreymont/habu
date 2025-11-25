#!/usr/bin/env sbcl --script
;;; Test catch/throw dynamic non-local exits via run-bytecode runtime.

(load "run-habu.lisp")

(defun run-test (name forms expected)
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (format t "~A: pass~%" name)
        (progn
          (format t "~A: FAIL (expected ~A, got ~A)~%" name expected result)
          (sb-ext:quit :unix-status 1)))))

;; Test 1: Catch returns body value normally
(run-test "catch-normal"
          '((catch 'tag #x5))
          #x5)

;; Test 2: Catch with throw exits early
(run-test "catch-throw"
          '((catch 'tag
              (throw 'tag #x7)
              #x99))
          #x7)

;; Test 3: Throw in conditional
(run-test "catch-throw-cond"
          '((catch 'done
              (if (= #x1 #x1)
                  (throw 'done #xA)
                  #x0)
              #x99))
          #xA)

;; Test 4: Nested catch with inner throw
(run-test "catch-nested-inner"
          '((catch 'outer
              (catch 'inner
                (throw 'inner #x5)
                #x1)
              #x2))
          #x2)

;; Test 5: Nested catch with outer throw
(run-test "catch-nested-outer"
          '((catch 'outer
              (catch 'inner
                (throw 'outer #x10)
                #x1)
              #x2))
          #x10)

;; Test 6: Throw with expression value
(run-test "catch-throw-expr"
          '((catch 'tag
              (throw 'tag (+ #x3 #x4))
              #x0))
          #x7)

(format t "All catch/throw tests passed~%")
(sb-ext:quit :unix-status 0)
