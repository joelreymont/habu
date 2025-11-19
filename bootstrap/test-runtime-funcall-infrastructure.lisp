;;;; Runtime Funcall Infrastructure Tests
;;;; Verify that defun creates alien-callables and stores function pointers correctly

(load "compiler.lisp")
(in-package :habu-compiler)

(initialize-runtime-integration)

(format t "~%Runtime Funcall Infrastructure Tests~%")
(format t "=====================================~%~%")

(defvar *test-count* 0)
(defvar *pass-count* 0)

(defun test (name condition &optional message)
  (incf *test-count*)
  (if condition
      (progn
        (incf *pass-count*)
        (format t "[32m✓[0m ~A~%" name))
      (progn
        (format t "[31m✗[0m ~A" name)
        (when message
          (format t ": ~A" message))
        (format t "~%"))))

;; Test 1: Verify defun creates symbol and stores function pointer
(format t "~%[34m1. Symbol and Function Pointer Creation[0m~%")
(format t "========================================~%")

(compile-expression '(defun test-fn (x) (+ x 1)) :arch :x86_64)

(let* ((sym-table (symbol-value (find-symbol "*SYMBOL-TABLE*" :habu-runtime)))
       (test-sym (gethash "TEST-FN" sym-table))
       (get-fn (find-symbol "RUNTIME-SYMBOL-FUNCTION" :habu-runtime)))
  (test "SYMBOL-CREATED" (not (null test-sym)))
  (when test-sym
    (let ((fn-ptr (funcall get-fn test-sym)))
      (test "FUNCTION-POINTER-SET" (not (zerop fn-ptr))
            (format nil "Expected non-zero, got ~X" fn-ptr))
      (format t "   Function pointer: ~X~%" fn-ptr))))

;; Test 2: Verify alien-callable was created
(format t "~%[34m2. Alien-Callable Creation[0m~%")
(format t "===========================~%")

(let ((callable-name (intern "HABU-FUNCTION-TEST-FN" :habu-compiler)))
  (test "ALIEN-CALLABLE-EXISTS" (fboundp callable-name)))

;; Test 3: Different arities
(format t "~%[34m3. Multiple Arities[0m~%")
(format t "===================~%")

(compile-expression '(defun zero-args () 42) :arch :x86_64)
(compile-expression '(defun one-arg (x) x) :arch :x86_64)
(compile-expression '(defun two-args (x y) (+ x y)) :arch :x86_64)
(compile-expression '(defun three-args (x y z) (+ x (+ y z))) :arch :x86_64)

(let ((sym-table (symbol-value (find-symbol "*SYMBOL-TABLE*" :habu-runtime)))
      (get-fn (find-symbol "RUNTIME-SYMBOL-FUNCTION" :habu-runtime)))
  (dolist (fn-name '("ZERO-ARGS" "ONE-ARG" "TWO-ARGS" "THREE-ARGS"))
    (let* ((sym (gethash fn-name sym-table))
           (fn-ptr (when sym (funcall get-fn sym))))
      (test (format nil "~A-CREATED" fn-name)
            (and sym (not (zerop fn-ptr)))))))

;; Test 4: Funcall code generation
(format t "~%[34m4. Funcall Code Generation[0m~%")
(format t "===========================~%")

(let ((code (compile-expression '(funcall 'test-fn 5) :arch :x86_64)))
  (test "FUNCALL-GENERATES-CODE" (> (length code) 0))
  (format t "   Generated ~D bytes of machine code~%" (length code)))

;; Test 5: Funcall with different arities
(format t "~%[34m5. Funcall Code for Different Arities[0m~%")
(format t "=======================================~%")

(let ((code0 (compile-expression '(funcall 'zero-args) :arch :x86_64))
      (code1 (compile-expression '(funcall 'one-arg 5) :arch :x86_64))
      (code2 (compile-expression '(funcall 'two-args 3 4) :arch :x86_64))
      (code3 (compile-expression '(funcall 'three-args 1 2 3) :arch :x86_64)))
  (test "FUNCALL-0-ARGS" (> (length code0) 0))
  (test "FUNCALL-1-ARG" (> (length code1) 0))
  (test "FUNCALL-2-ARGS" (> (length code2) 0))
  (test "FUNCALL-3-ARGS" (> (length code3) 0))
  (format t "   Code sizes: 0-args=~D, 1-arg=~D, 2-args=~D, 3-args=~D bytes~%"
          (length code0) (length code1) (length code2) (length code3)))

;; Test 6: Symbol-function slot offset
(format t "~%[34m6. Symbol Structure Verification[0m~%")
(format t "=================================~%")

(let* ((sym-table (symbol-value (find-symbol "*SYMBOL-TABLE*" :habu-runtime)))
       (test-sym (gethash "TEST-FN" sym-table))
       (heap (symbol-value (find-symbol "*HEAP*" :habu-runtime)))
       (read-u64 (find-symbol "READ-U64" :habu-runtime)))
  (when test-sym
    (let ((fn-slot-value (funcall read-u64 heap (+ test-sym 24))))
      (test "FUNCTION-SLOT-AT-OFFSET-24" (not (zerop fn-slot-value)))
      (format t "   Symbol address: ~X~%" test-sym)
      (format t "   Function slot value: ~X~%" fn-slot-value))))

;; Test 7: Multiple functions don't interfere
(format t "~%[34m7. Multiple Function Definitions[0m~%")
(format t "=================================~%")

(compile-expression '(defun fn-a () 1) :arch :x86_64)
(compile-expression '(defun fn-b () 2) :arch :x86_64)
(compile-expression '(defun fn-c () 3) :arch :x86_64)

(let ((sym-table (symbol-value (find-symbol "*SYMBOL-TABLE*" :habu-runtime)))
      (get-fn (find-symbol "RUNTIME-SYMBOL-FUNCTION" :habu-runtime)))
  (let ((fn-a-ptr (funcall get-fn (gethash "FN-A" sym-table)))
        (fn-b-ptr (funcall get-fn (gethash "FN-B" sym-table)))
        (fn-c-ptr (funcall get-fn (gethash "FN-C" sym-table))))
    (test "ALL-FUNCTIONS-HAVE-POINTERS"
          (and (not (zerop fn-a-ptr))
               (not (zerop fn-b-ptr))
               (not (zerop fn-c-ptr))))
    (test "FUNCTION-POINTERS-ARE-UNIQUE"
          (and (/= fn-a-ptr fn-b-ptr)
               (/= fn-b-ptr fn-c-ptr)
               (/= fn-a-ptr fn-c-ptr)))
    (format t "   FN-A: ~X, FN-B: ~X, FN-C: ~X~%" fn-a-ptr fn-b-ptr fn-c-ptr)))

;; Summary
(format t "~%=====================================~%")
(format t "  Test Results~%")
(format t "=====================================~%")
(format t "Total:  ~D~%" *test-count*)
(format t "Passed: [32m~D/~D[0m~%" *pass-count* *test-count*)
(format t "~%")

(when (< *pass-count* *test-count*)
  (format t "[31mSome tests failed![0m~%")
  (sb-ext:quit :unix-status 1))

(format t "[32mAll infrastructure tests passed![0m~%")
(format t "~%[34mNote:[0m These tests verify the runtime funcall infrastructure.~%")
(format t "Actual execution testing requires integration tests with compiled binaries.~%")

(sb-ext:quit)
