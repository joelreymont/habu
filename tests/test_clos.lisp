#!/usr/bin/env sbcl --script
;;; Tests for basic CLOS: defclass, make-instance, slot-value, typep, class-of

(load "run-habu.lisp")

(defun run-test (name forms expected)
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (format t "~A: pass~%" name)
        (progn
          (format t "~A: FAIL (expected ~X, got ~X)~%" name expected result)
          (sb-ext:quit :unix-status 1)))))

(format t "~%=== CLOS Tests ===~%~%")

;; Test 1: defclass and make-instance with no initargs
(format t "Test 1 - defclass and make-instance:~%")
(run-test "defclass-basic"
          '((defclass point ()
              ((x :initform #x0)
               (y :initform #x0)))
            (let ((p (make-instance 'point)))
              (if (vectorp p) #x1 #x0)))
          #x1)

;; Test 2: make-instance with initargs
(format t "~%Test 2 - make-instance with initargs:~%")
(run-test "make-instance-initargs"
          '((defclass point ()
              ((x :initform #x0)
               (y :initform #x0)))
            (let ((p (make-instance 'point :x #x5 :y #xA)))
              (point-x p)))
          #x5)

;; Test 3: Accessor functions
(format t "~%Test 3 - accessor functions:~%")
(run-test "accessor-y"
          '((defclass point ()
              ((x :initform #x0)
               (y :initform #x0)))
            (let ((p (make-instance 'point :x #x3 :y #x7)))
              (point-y p)))
          #x7)

;; Test 4: slot-value
(format t "~%Test 4 - slot-value:~%")
(run-test "slot-value"
          '((defclass point ()
              ((x :initform #x0)
               (y :initform #x0)))
            (let ((p (make-instance 'point :x #x10 :y #x20)))
              (slot-value p 'y)))
          #x20)

;; Test 5: class-of
(format t "~%Test 5 - class-of:~%")
(run-test "class-of"
          '((defclass point ()
              ((x :initform #x0)
               (y :initform #x0)))
            (let ((p (make-instance 'point)))
              (if (eq (class-of p) 'point) #x1 #x0)))
          #x1)

;; Test 6: typep
(format t "~%Test 6 - typep:~%")
(run-test "typep"
          '((defclass point ()
              ((x :initform #x0)
               (y :initform #x0)))
            (let ((p (make-instance 'point)))
              (if (typep p 'point) #x1 #x0)))
          #x1)

;; Test 7: Predicate function
(format t "~%Test 7 - predicate function:~%")
(run-test "predicate"
          '((defclass point ()
              ((x :initform #x0)
               (y :initform #x0)))
            (let ((p (make-instance 'point)))
              (if (point-p p) #x1 #x0)))
          #x1)

;; Test 8: Multiple classes
(format t "~%Test 8 - multiple classes:~%")
(run-test "multiple-classes"
          '((defclass point ()
              ((x :initform #x0)
               (y :initform #x0)))
            (defclass rectangle ()
              ((width :initform #x0)
               (height :initform #x0)))
            (let* ((p (make-instance 'point :x #x1))
                   (r (make-instance 'rectangle :width #x10)))
              (+ (point-x p) (rectangle-width r))))
          #x11)  ; 1 + 16 = 17

;; Test 9: Default initforms
(format t "~%Test 9 - default initforms:~%")
(run-test "default-initforms"
          '((defclass counter ()
              ((value :initform #x42)))
            (let ((c (make-instance 'counter)))
              (counter-value c)))
          #x42)

;; Test 10: Class with no slots
(format t "~%Test 10 - class with no slots:~%")
(run-test "no-slots"
          '((defclass marker ()
              ())
            (let ((m (make-instance 'marker)))
              (if (marker-p m) #x1 #x0)))
          #x1)

(format t "~%=== All CLOS Tests Passed ===~%")
(sb-ext:quit :unix-status 0)
