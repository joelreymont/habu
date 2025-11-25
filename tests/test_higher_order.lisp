#!/usr/bin/env sbcl --script
;;; Test higher-order functions via run-bytecode runtime.

(load "run-habu.lisp")

(defun run-test (name forms expected)
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (format t "~A: pass~%" name)
        (progn
          (format t "~A: FAIL (expected ~A, got ~A)~%" name expected result)
          (sb-ext:quit :unix-status 1)))))

;; Mapcar tests
(run-test "mapcar-empty"
          '((null (mapcar (lambda (x) (+ x #x1)) #x0)))
          #x1)  ; empty list returns nil

(run-test "mapcar-single"
          '((car (mapcar (lambda (x) (+ x #x1)) (list #x5))))
          #x6)  ; 5 + 1 = 6

(run-test "mapcar-multiple"
          '((car (mapcar (lambda (x) (* x #x2)) (list #x1 #x2 #x3))))
          #x2)  ; first element: 1 * 2 = 2

(run-test "mapcar-length"
          '((length (mapcar (lambda (x) x) (list #x1 #x2 #x3))))
          #x3)  ; same length

(run-test "mapcar-second"
          '((cadr (mapcar (lambda (x) (+ x #x10)) (list #x1 #x2 #x3))))
          #x12)  ; second element: 2 + 16 = 18

;; Mapc tests (returns original list, executes for side effects)
(run-test "mapc-returns-list"
          '((car (mapc (lambda (x) x) (list #x1 #x2 #x3))))
          #x1)  ; returns original list

(run-test "mapc-empty"
          '((null (mapc (lambda (x) x) #x0)))
          #x1)  ; empty list returns nil

;; Reduce tests
(run-test "reduce-sum"
          '((reduce (lambda (a b) (+ a b)) (list #x1 #x2 #x3 #x4)))
          #xA)  ; 1+2+3+4 = 10

(run-test "reduce-with-init"
          '((reduce (lambda (a b) (+ a b)) (list #x1 #x2 #x3) #x10))
          #x16)  ; 16+1+2+3 = 22

(run-test "reduce-product"
          '((reduce (lambda (a b) (* a b)) (list #x2 #x3 #x4)))
          #x18)  ; 2*3*4 = 24

(run-test "reduce-single"
          '((reduce (lambda (a b) (+ a b)) (list #x42)))
          #x42)  ; single element

;; Combined higher-order
(run-test "mapcar-reduce"
          '((reduce (lambda (a b) (+ a b))
                    (mapcar (lambda (x) (* x x)) (list #x1 #x2 #x3))))
          #xE)  ; 1 + 4 + 9 = 14

(format t "All higher-order function tests passed~%")
(sb-ext:quit :unix-status 0)
