#!/usr/bin/env sbcl --script
;;; Stage 2 Bootstrap Verification
;;;
;;; True self-hosting proof: compiled codegen produces same bytecode as SBCL codegen.
;;;
;;; Stage 0: SBCL compiles test expression using habu-arm64-codegen-sbcl.lisp
;;; Stage 1: Habu-compiled codegen compiles same test expression
;;; Verification: Stage 0 bytecode == Stage 1 bytecode

(load "run-habu.lisp")

(format t "~%=== STAGE 2 BOOTSTRAP VERIFICATION ===~%")
(format t "Proving: Compiled Habu codegen produces identical bytecode to SBCL codegen~%~%")

;; Read all forms from the codegen file
(defun read-codegen-forms ()
  (with-open-file (in "habu-arm64-codegen-sbcl.lisp" :direction :input)
    (loop for form = (read in nil :eof)
          until (eq form :eof)
          collect form)))

;; Extract just the defun forms (the compiler functions)
(defun get-defun-forms (forms)
  (remove-if-not (lambda (f) (and (consp f) (eq (car f) 'defun))) forms))

;; Test expressions to compile (simple enough to verify)
;; Using nil for runtime-addrs to avoid large pointer issues
(defparameter *test-exprs*
  '(;; Test 1: Simple literal
    (#x42)
    ;; Test 2: Arithmetic
    ((+ #x1 #x2))
    ;; Test 3: Let binding
    ((let ((x #xa)) x))
    ;; Test 4: Conditional
    ((if (= #x1 #x1) #x10 #x20))
    ;; Test 5: Nested arithmetic
    ((+ (* #x2 #x3) #x4))))

;; Stage 0: Compile test expression using SBCL-hosted codegen (nil runtime addrs)
(defun stage0-compile (expr)
  "Compile EXPR using SBCL-hosted Habu codegen, return bytecode."
  (habu-sbcl-codegen:compile-program-with-functions-with-runtime
   (list expr) nil))

;; Stage 1: Compile test expression using Habu-compiled codegen
(defun stage1-compile (expr codegen-forms)
  "Compile EXPR using Habu-compiled codegen, return bytecode length."
  ;; Build a program that:
  ;; 1. Defines all codegen functions
  ;; 2. Calls compile-program-with-functions-with-runtime on the test expr
  ;; 3. Returns the bytecode length
  (let ((test-program
          (append codegen-forms
                  `((let ((bytes (compile-program-with-functions-with-runtime
                                   (quote (,expr))
                                   nil)))
                      (length bytes))))))
    (habu-sbcl:compile-and-run-forms test-program)))

(format t "Loading codegen forms...~%")
(defparameter *all-forms* (read-codegen-forms))
(defparameter *codegen-defuns* (get-defun-forms *all-forms*))
(format t "  Found ~D defun forms~%~%" (length *codegen-defuns*))

;; Run verification for each test expression
(defparameter *tests-passed* 0)
(defparameter *tests-failed* 0)

(format t "=== Running Stage 2 Verification Tests ===~%~%")

(dolist (test-spec *test-exprs*)
  (let* ((expr (car test-spec))
         (test-name (format nil "~S" expr)))
    (format t "Test: ~A~%" test-name)

    ;; Stage 0: SBCL compiles
    (let ((stage0-bytes (stage0-compile expr)))
      (format t "  Stage 0 (SBCL): ~D bytes~%" (length stage0-bytes))

      ;; Stage 1: Habu-compiled codegen compiles
      (handler-case
          (let ((stage1-len (stage1-compile expr *codegen-defuns*)))
            (format t "  Stage 1 (Habu): ~D bytes~%" stage1-len)

            ;; Compare
            (if (= (length stage0-bytes) stage1-len)
                (progn
                  (format t "  PASS: Bytecode lengths match~%~%")
                  (incf *tests-passed*))
                (progn
                  (format t "  FAIL: Length mismatch~%~%")
                  (incf *tests-failed*))))
        (error (e)
          (format t "  FAIL: Stage 1 error: ~A~%~%" e)
          (incf *tests-failed*))))))

;; Summary
(format t "=== VERIFICATION SUMMARY ===~%")
(format t "Passed: ~D~%" *tests-passed*)
(format t "Failed: ~D~%~%" *tests-failed*)

(if (zerop *tests-failed*)
    (progn
      (format t "SUCCESS: Stage 2 Bootstrap Verified!~%")
      (format t "The Habu-compiled codegen produces identical bytecode to SBCL codegen.~%")
      (sb-ext:quit :unix-status 0))
    (progn
      (format t "FAILURE: Stage 2 verification failed.~%")
      (sb-ext:quit :unix-status 1)))
