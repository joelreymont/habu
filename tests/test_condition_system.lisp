#!/usr/bin/env sbcl --script
;;; Tests for condition system: handler-case, signal, restart-case, invoke-restart
;;; Note: Due to Habu's flag-based catch/throw, signal must be in tail position
;;; or have no code after it in the same progn for proper non-local exit.

(load "run-habu.lisp")

(defun run-test (name forms expected)
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (format t "~A: pass~%" name)
        (progn
          (format t "~A: FAIL (expected ~X, got ~X)~%" name expected result)
          (sb-ext:quit :unix-status 1)))))

(format t "~%=== Condition System Tests ===~%~%")

;; Test 1: handler-case with no signal (normal return)
(format t "Test 1 - handler-case no signal:~%")
(run-test "handler-case-normal"
          '((handler-case
               (+ #x1 #x2)
             (my-error (e) #xFF)))
          #x3)

;; Test 2: signal with matching handler (signal in tail position)
(format t "~%Test 2 - signal with matching handler:~%")
(run-test "signal-match"
          '((handler-case
               (signal 'my-error #x42)
             (my-error (e) (+ e #x10))))
          #x52)  ; 0x42 + 0x10 = 0x52

;; Test 3: signal with no matching handler (returns signal value)
(format t "~%Test 3 - signal with no matching handler:~%")
(run-test "signal-no-match"
          '((handler-case
               (signal 'other-error #x42)
             (my-error (e) #xFF)))
          #x42)  ; Signal returns its value when no handler matches

;; Test 4: handler-case with catch-all handler (t)
(format t "~%Test 4 - handler-case catch-all:~%")
(run-test "handler-case-catch-all"
          '((handler-case
               (signal 'any-error #x77)
             (t (e) (+ e #x1))))
          #x78)  ; 0x77 + 0x1 = 0x78

;; Test 5: nested handler-case (inner handles)
(format t "~%Test 5 - nested handler-case (inner handles):~%")
(run-test "nested-handler-inner"
          '((handler-case
               (handler-case
                   (signal 'inner-error #xAA)
                 (inner-error (e) (+ e #x1)))
             (outer-error (e) #xFF)))
          #xAB)  ; 0xAA + 0x1 = 0xAB

;; Test 6: restart-case with no invoke (normal return)
(format t "~%Test 6 - restart-case no invoke:~%")
(run-test "restart-case-normal"
          '((restart-case
               (+ #x5 #x5)
             (use-value (v) v)))
          #xA)

;; Test 7: restart-case with invoke-restart (in tail position)
(format t "~%Test 7 - restart-case with invoke-restart:~%")
(run-test "restart-invoke"
          '((restart-case
               (invoke-restart 'use-value #x42)
             (use-value (v) (+ v #x10))))
          #x52)  ; 0x42 + 0x10 = 0x52

;; Test 8: handler-case without var binding
(format t "~%Test 8 - handler-case without var binding:~%")
(run-test "handler-no-var"
          '((handler-case
               (signal 'simple-error)
             (simple-error () #xBB)))
          #xBB)

;; Test 9: multiple handlers (first match wins)
(format t "~%Test 9 - multiple handlers:~%")
(run-test "multiple-handlers"
          '((handler-case
               (signal 'second-type #x22)
             (first-type (e) #x11)
             (second-type (e) e)
             (third-type (e) #x33)))
          #x22)

;; Test 10: signal without value (handler gets nil = 0)
(format t "~%Test 10 - signal without value:~%")
(run-test "signal-no-value"
          '((handler-case
               (signal 'no-val-error)
             (no-val-error (e) #xDD)))
          #xDD)

(format t "~%=== All Condition System Tests Passed ===~%")
(sb-ext:quit :unix-status 0)
