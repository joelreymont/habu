#!/usr/bin/env sbcl --script
;;; Tests for defmethod: generic function dispatch on class types

(load "run-habu.lisp")

(defun run-test (name forms expected)
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (format t "~A: pass~%" name)
        (progn
          (format t "~A: FAIL (expected ~X, got ~X)~%" name expected result)
          (sb-ext:quit :unix-status 1)))))

(format t "~%=== defmethod Tests ===~%~%")

;; Test 1: Simple method dispatch on single class
(format t "Test 1 - single method dispatch:~%")
(run-test "single-method"
          '((defclass point ()
              ((x :initform #x0)
               (y :initform #x0)))
            (defgeneric get-value (obj))
            (defmethod get-value ((obj point))
              (+ (point-x obj) (point-y obj)))
            (let ((p (make-instance 'point :x #x3 :y #x5)))
              (get-value p)))
          #x8)

;; Test 2: Multiple methods, different classes
(format t "~%Test 2 - multiple methods:~%")
(run-test "multiple-methods"
          '((defclass point ()
              ((x :initform #x0)
               (y :initform #x0)))
            (defclass rect ()
              ((w :initform #x0)
               (h :initform #x0)))
            (defgeneric area (obj))
            (defmethod area ((obj point))
              #x0)  ; Points have no area
            (defmethod area ((obj rect))
              (* (rect-w obj) (rect-h obj)))
            (let* ((p (make-instance 'point :x #x1 :y #x2))
                   (r (make-instance 'rect :w #x4 :h #x5)))
              (+ (area p) (area r))))
          #x14)  ; 0 + 4*5 = 20 = 0x14

;; Test 3: Method with multiple parameters
(format t "~%Test 3 - method with multiple params:~%")
(run-test "multi-param-method"
          '((defclass point ()
              ((x :initform #x0)
               (y :initform #x0)))
            (defgeneric move (obj dx dy))
            (defmethod move ((obj point) dx dy)
              (+ (point-x obj) dx dy))
            (let ((p (make-instance 'point :x #x10)))
              (move p #x5 #x3)))
          #x18)  ; 16 + 5 + 3 = 24 = 0x18

;; Test 4: No matching method returns 0
(format t "~%Test 4 - no matching method:~%")
(run-test "no-match"
          '((defclass point ()
              ((x :initform #x0)))
            (defclass other ()
              ((val :initform #x0)))
            (defgeneric process (obj))
            (defmethod process ((obj point))
              (point-x obj))
            (let ((o (make-instance 'other :val #x42)))
              (process o)))
          #x0)  ; No method for 'other, returns 0

;; Test 5: Dispatch without defgeneric (implicitly created)
(format t "~%Test 5 - implicit generic:~%")
(run-test "implicit-generic"
          '((defclass counter ()
              ((count :initform #x0)))
            (defmethod tick ((c counter))
              (+ (counter-count c) #x1))
            (let ((c (make-instance 'counter :count #x9)))
              (tick c)))
          #xA)  ; 9 + 1 = 10 = 0xA

;; Test 6: Two classes same method name (dispatch correctness)
(format t "~%Test 6 - dispatch to correct method:~%")
(run-test "dispatch-correct"
          '((defclass foo ()
              ((val :initform #x0)))
            (defclass bar ()
              ((val :initform #x0)))
            (defgeneric compute (obj))
            (defmethod compute ((obj foo))
              (* (foo-val obj) #x2))  ; double
            (defmethod compute ((obj bar))
              (* (bar-val obj) #x3))  ; triple
            (let* ((f (make-instance 'foo :val #x10))
                   (b (make-instance 'bar :val #x10)))
              (+ (compute f) (compute b))))
          #x50)  ; (16*2) + (16*3) = 32 + 48 = 80 = 0x50

(format t "~%=== All defmethod Tests Passed ===~%")
(sb-ext:quit :unix-status 0)
