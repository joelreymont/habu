;; Test native compilation of IR traversal patterns
;; These tests verify IR traversal patterns used in the Habu compiler work natively
(require :asdf)
(push (truename "bootstrap/") asdf:*central-registry*)
(asdf:load-system :habu)

(defpackage :habu-test-native-ir-traversal
  (:use :cl))

(in-package :habu-test-native-ir-traversal)
(load "bootstrap/compiler.lisp")
(load "bootstrap/macho.lisp")

(format t "~%=== Test native IR traversal patterns ===~%~%")

(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

(defun test-native (name source expected)
  "Compile source to native executable and verify exit code"
  (let ((path (format nil "/tmp/nit_~A" name)))
    (handler-case
        (progn
          (habu:deliver source path)
          (sb-ext:run-program "/usr/bin/codesign" (list "-s" "-" path)
                              :output nil :error nil :wait t)
          (let* ((proc (sb-ext:run-program path nil :output nil :error nil :wait t))
                 (code (sb-ext:process-exit-code proc)))
            (if (= code expected)
                (progn
                  (format t "[PASS] ~A = ~A~%" name code)
                  (incf *tests-passed*))
                (progn
                  (format t "[FAIL] ~A: expected ~A, got ~A~%" name expected code)
                  (incf *tests-failed*)))))
      (error (e)
        (format t "[ERR]  ~A: ~A~%" name e)
        (incf *tests-failed*)))))

;; Test 1: Simple list traversal (like collect-var-offsets)
(test-native "list-traverse"
  "(defun list-len (lst)
     (if (null lst)
         0
         (+ 1 (list-len (cdr lst)))))
   (list-len (cons 1 (cons 2 (cons 3 nil))))"
  3)

;; Test 2: Tree node counter (like count-instrs)
(test-native "tree-count"
  "(defun count-nodes (tree)
     (if (consp tree)
         (+ 1 (count-nodes (car tree)) (count-nodes (cdr tree)))
         0))
   (count-nodes (cons (cons 1 2) (cons 3 4)))"
  3)

;; Test 3: IR node visitor (like collect-called-functions-from-ir)
(test-native "ir-visitor"
  "(defun count-lit (ir)
     (if (consp ir)
         (if (eq (car ir) 'lit)
             1
             (+ (count-lit (car ir)) (count-lit (cdr ir))))
         0))
   (let ((ir (cons 'add (cons (cons 'lit (cons 10 nil))
                              (cons (cons 'lit (cons 20 nil)) nil)))))
     (count-lit ir))"
  2)

;; Test 4: Free variable collection (like find-free-vars)
(test-native "free-vars"
  "(defun my-member (x lst)
     (if (null lst)
         nil
         (if (eq x (car lst))
             t
             (my-member x (cdr lst)))))
   (defun collect-vars (expr bound)
     (if (symbolp expr)
         (if (my-member expr bound)
             nil
             (cons expr nil))
         (if (consp expr)
             (let ((op (car expr)))
               (if (eq op 'let)
                   (let ((name (car (car (cdr expr)))))
                     (collect-vars (car (cdr (cdr expr))) (cons name bound)))
                   (if (eq op 'lit)
                       nil
                       (append (collect-vars (car expr) bound)
                               (collect-vars (cdr expr) bound)))))
             nil)))
   (let ((free (collect-vars 'x nil)))
     (if (consp free) 42 0))"
  42)

;; Test 5: Lambda lifting detection (is expression a lambda?)
(test-native "lambda-detect"
  "(defun is-lambda (expr)
     (if (consp expr)
         (if (eq (car expr) 'lambda) 1 0)
         0))
   (+ (is-lambda (cons 'lambda nil))
      (is-lambda (cons 'let nil))
      (is-lambda 42))"
  1)

;; Test 6: Function call extraction from IR
(test-native "call-extract"
  "(defun my-member (x lst)
     (if (null lst)
         nil
         (if (eq x (car lst))
             t
             (my-member x (cdr lst)))))
   (defun get-calls (ir calls)
     (if (consp ir)
         (let ((tag (car ir)))
           (if (eq tag 'call-fn)
               (let ((name (car (cdr ir))))
                 (if (my-member name calls)
                     (get-calls (cdr (cdr ir)) calls)
                     (get-calls (cdr (cdr ir)) (cons name calls))))
               (get-calls (cdr ir) (get-calls (car ir) calls))))
         calls))
   (let* ((ir (cons 'call-fn (cons 'foo (cons 'call-fn (cons 'bar nil)))))
          (calls (get-calls ir nil)))
     (+ (if (my-member 'foo calls) 1 0)
        (if (my-member 'bar calls) 1 0)))"
  2)

;; Test 7: List mapping (like mapcar in compiler)
(test-native "list-map"
  "(defun my-mapcar (f lst)
     (if (null lst)
         nil
         (cons (funcall f (car lst))
               (my-mapcar f (cdr lst)))))
   (defun add1 (x) (+ x 1))
   (let ((result (my-mapcar #'add1 (cons 10 (cons 20 nil)))))
     (+ (car result) (car (cdr result))))"
  32)

;; Test 8: Environment chain walking (like nc-env-lookup depth)
(test-native "env-chain"
  "(defun walk-env (name env depth)
     (if (null env)
         depth
         (if (eq name (car (car env)))
             depth
             (walk-env name (cdr env) (+ depth 1)))))
   (let ((env (cons (cons 'x 0) (cons (cons 'y 1) (cons (cons 'z 2) nil)))))
     (+ (walk-env 'x env 0)
        (walk-env 'y env 0)
        (walk-env 'z env 0)))"
  3)  ; 0 + 1 + 2

;; Test 9: List accumulator (like build-call-graph)
(test-native "list-accum"
  "(defun my-reverse (lst acc)
     (if (null lst)
         acc
         (my-reverse (cdr lst) (cons (car lst) acc))))
   (let ((rev (my-reverse (cons 1 (cons 2 (cons 3 nil))) nil)))
     (car rev))"
  3)

;; Test 10: Deep nested list access (like accessing IR arguments)
(test-native "deep-access"
  "(defun get-deep (x)
     (car (cdr (cdr (cdr (car (cdr x)))))))
   (let ((ir (cons 'call (cons (cons 'args (cons 'a (cons 'b (cons 42 nil)))) nil))))
     (get-deep ir))"
  42)

(format t "~%Results: ~A passed, ~A failed~%~%" *tests-passed* *tests-failed*)

(if (> *tests-failed* 0)
    (sb-ext:quit :unix-status 1)
    (sb-ext:quit :unix-status 0))
