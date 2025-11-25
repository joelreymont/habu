#!/usr/bin/env sbcl --script
;;; Test tagbody/go control flow via run-bytecode runtime.
;;; NOTE: Due to closure mutation limitations, tests use accumulator patterns
;;; instead of setq of outer variables.

(load "run-habu.lisp")

(defun run-test (name forms expected)
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (format t "~A: pass~%" name)
        (progn
          (format t "~A: FAIL (expected ~A, got ~A)~%" name expected result)
          (sb-ext:quit :unix-status 1)))))

;; Test 1: Simple tagbody returns nil (0)
(run-test "tagbody-return"
          '((tagbody))
          #x0)

;; Test 2: Tagbody with forms but no go
(run-test "tagbody-forms"
          '((tagbody (+ #x1 #x2)))
          #x0)

;; Test 3: Tagbody with tags but no go
(run-test "tagbody-tags-only"
          '((tagbody
             start
             middle
             end))
          #x0)

;; Test 4: defun using tagbody for loop
(run-test "tagbody-defun-loop"
          '((defun count-to (n)
              (let ((result #x0))
                (tagbody
                 loop
                 (if (< result n)
                     (progn
                       (setq result (+ result #x1))
                       (go loop))))
                result))
            (count-to #x5))
          #x5)

;; Test 5: defun with go forward skip
(run-test "tagbody-defun-skip"
          '((defun skip-middle ()
              (let ((x #x1))
                (tagbody
                 (setq x (+ x #x10))
                 (go end)
                 middle
                 (setq x (+ x #x100))
                 end)
                x))
            (skip-middle))
          #x11)

;; Test 6: Integer tags in defun
(run-test "tagbody-int-tags-defun"
          '((defun with-int-tags ()
              (let ((x #x0))
                (tagbody
                 (setq x #x1)
                 (go 2)
                 1
                 (setq x (+ x #x10))
                 (go 3)
                 2
                 (setq x (+ x #x100))
                 (go 1)
                 3)
                x))
            (with-int-tags))
          #x111)

(format t "All tagbody/go tests passed~%")
(sb-ext:quit :unix-status 0)
