#!/usr/bin/env sbcl --script
;;; Test defstruct macro expansion and generated functions.

(load "run-habu.lisp")

(defun run-test (name forms expected)
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (format t "~A: pass~%" name)
        (progn
          (format t "~A: FAIL (expected ~A, got ~A)~%" name expected result)
          (sb-ext:quit :unix-status 1)))))

;; Test 1: Basic vector operations (prerequisite)
(run-test "make-vector"
          '((let ((v (make-vector #x3)))
              (vector-length v)))
          #x3)

;; Test 2: Vector set and ref
(run-test "vector-set-ref"
          '((let ((v (make-vector #x2)))
              (vector-set v #x0 #x42)
              (vector-ref v #x0)))
          #x42)

;; Test 3: Simple defstruct constructor
(run-test "defstruct-make"
          '((defstruct point x y)
            (let ((p (make-point :x #x10 :y #x20)))
              (point-x p)))
          #x10)

;; Test 4: Access second slot
(run-test "defstruct-slot2"
          '((defstruct point x y)
            (let ((p (make-point :x #x10 :y #x20)))
              (point-y p)))
          #x20)

;; Test 5: Predicate true
(run-test "defstruct-predicate-true"
          '((defstruct point x y)
            (let ((p (make-point :x #x1 :y #x2)))
              (if (point-p p) #x1 #x0)))
          #x1)

;; Test 6: Predicate false for non-struct
(run-test "defstruct-predicate-false"
          '((defstruct point x y)
            (if (point-p (cons #x1 #x2)) #x1 #x0))
          #x0)

;; Test 7: Multiple fields
(run-test "defstruct-3-fields"
          '((defstruct triple a b c)
            (let ((t (make-triple :a #x1 :b #x2 :c #x3)))
              (+ (triple-a t) (+ (triple-b t) (triple-c t)))))
          #x6)

;; Test 8: Constructor with defaults (nil)
(run-test "defstruct-default-nil"
          '((defstruct point x y)
            (let ((p (make-point :x #x5)))
              (if (point-y p) #x1 #x0)))
          #x0)

(format t "All defstruct tests passed~%")
(sb-ext:quit :unix-status 0)
