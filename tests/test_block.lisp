#!/usr/bin/env sbcl --script
;;; Test block/return-from non-local exits via run-bytecode runtime.

(load "run-habu.lisp")

(defun run-test (name forms expected)
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (format t "~A: pass~%" name)
        (progn
          (format t "~A: FAIL (expected ~A, got ~A)~%" name expected result)
          (sb-ext:quit :unix-status 1)))))

;; Test 1: Block returns body value normally
(run-test "block-normal"
          '((block test #x5))
          #x5)

;; Test 2: Block with multiple forms returns last
(run-test "block-multi"
          '((block test #x1 #x2 #x3))
          #x3)

;; Test 3: return-from exits early with value
(run-test "return-from-early"
          '((block test
              (return-from test #x7)
              #x99))
          #x7)

;; Test 4: return-from in conditional
(run-test "return-from-cond"
          '((block outer
              (if (= #x1 #x1)
                  (return-from outer #xA)
                  #x0)
              #x99))
          #xA)

;; Test 5: return-from default nil value
(run-test "return-from-nil"
          '((block test
              (return-from test)
              #x42))
          #x0)

;; Test 6: Nested blocks with different names
(run-test "block-nested"
          '((block outer
              (block inner
                (return-from outer #x10)
                #x1)
              #x2))
          #x10)

;; Test 7: Block with sequential checks
(run-test "block-sequential"
          '((block found
              (if (> #x3 #x5) (return-from found #x3) #x0)
              (if (> #x7 #x5) (return-from found #x7) #x0)
              #x0))
          #x7)

;; Test 8: Block with no return-from
(run-test "block-no-exit"
          '((+ #x1 (block test (+ #x2 #x3))))
          #x6)

(format t "All block/return-from tests passed~%")
(sb-ext:quit :unix-status 0)
