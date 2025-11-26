;; Test tree-shaking: verifies dead code elimination
;; Uses run-bytecode to test compiled output

(load "run-habu.lisp")

;; Test file with unused functions - tests/tree-shake-input.lisp
(defparameter *tree-shake-source*
  "(defun used-helper (x) (* x 2))
   (defun unused-1 (a b) (+ a b))
   (defun unused-2 (x) (* x x x))
   (defun factorial (n)
     (if (<= n 1) 1 (* n (factorial (1- n)))))
   (defun main ()
     (used-helper (factorial 5)))
   (main)")

(defparameter *tree-shake-test-file* "/tmp/tree-shake-test-input.lisp")
(defparameter *test-results* nil)

;; Write test source
(with-open-file (out *tree-shake-test-file*
                     :direction :output
                     :if-exists :supersede)
  (write-string *tree-shake-source* out))

;; Helper to parse forms from string
(defun read-forms-from-string (s)
  (with-input-from-string (in s)
    (loop for form = (read in nil nil)
          while form collect form)))

;; Test 1: Basic tree-shaking removes unused functions
(let* ((forms (read-forms-from-string *tree-shake-source*))
       (result (habu-sbcl-codegen:compile-forms forms))
       (all-fns (car result))
       (main-ir (cadr result))
       (graph (habu-sbcl-codegen:build-call-graph all-fns main-ir))
       (reachable (habu-sbcl-codegen:compute-reachable-functions graph '(:main)))
       (filtered (habu-sbcl-codegen:filter-functions-by-reachability all-fns reachable))
       (all-names (mapcar #'car all-fns))
       (filtered-names (mapcar #'car filtered)))
  (push (list "Test 1: All functions found"
              (= (length all-fns) 5))
        *test-results*)
  (push (list "Test 2: Reachable count is 3"
              (= (hash-table-count reachable) 4))  ; :main + 3 functions
        *test-results*)
  (push (list "Test 3: MAIN is reachable"
              (gethash 'main reachable))
        *test-results*)
  (push (list "Test 4: FACTORIAL is reachable"
              (gethash 'factorial reachable))
        *test-results*)
  (push (list "Test 5: USED-HELPER is reachable"
              (gethash 'used-helper reachable))
        *test-results*)
  (push (list "Test 6: UNUSED-1 is not reachable"
              (not (gethash 'unused-1 reachable)))
        *test-results*)
  (push (list "Test 7: UNUSED-2 is not reachable"
              (not (gethash 'unused-2 reachable)))
        *test-results*)
  (push (list "Test 8: Filtered has 3 functions"
              (= (length filtered) 3))
        *test-results*))

;; Test 9: Tree-shaken code produces correct output
(let* ((forms (habu-sbcl:read-forms-from-file *tree-shake-test-file*)))
  (multiple-value-bind (result output)
      (habu-sbcl:compile-and-run-forms forms)
    (push (list "Test 9: Tree-shaken output is 240"
                (= result 240))
          *test-results*)))

;; Print results
(format t "~%=== Tree-Shaking Tests ===~%")
(setf *test-results* (reverse *test-results*))
(let ((pass 0) (fail 0))
  (dolist (test *test-results*)
    (let ((name (car test))
          (result (cadr test)))
      (if result
          (progn (incf pass) (format t "PASS: ~A~%" name))
          (progn (incf fail) (format t "FAIL: ~A~%" name)))))
  (format t "~%~A/~A tests passed~%" pass (+ pass fail)))

;; Cleanup
(delete-file *tree-shake-test-file*)
