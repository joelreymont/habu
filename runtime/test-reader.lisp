;;;; Test reader/printer functions

(load "memory.lisp")
(load "symbols.lisp")
(load "strings.lisp")
(load "reader.lisp")

(in-package :habu-runtime)

(initialize-runtime)

(format t "~%Testing Reader/Printer~%")
(format t "======================~%~%")

(defvar *test-count* 0)
(defvar *pass-count* 0)

(defun test (name condition &optional message)
  (incf *test-count*)
  (if condition
      (progn
        (incf *pass-count*)
        (format t "PASS ~A~%" name))
      (progn
        (format t "FAIL ~A" name)
        (when message
          (format t ": ~A" message))
        (format t "~%"))))

;; Test 1: Print fixnum
(format t "~%1. Print Fixnum~%")
(format t "==================~%")

(let* ((fixnum-42 (ash 42 4))  ; Tagged fixnum 42
       (result-ptr (runtime-print-to-string fixnum-42))
       (result-str (runtime-string->lisp result-ptr)))
  (test "PRINT-FIXNUM" (string= result-str "42"))
  (format t "   Result: ~S~%" result-str))

;; Test 2: Print string
(format t "~%2. Print String~%")
(format t "==================~%")

(let* ((str-ptr (runtime-lisp->string "hello"))
       (result-ptr (runtime-print-to-string str-ptr))
       (result-str (runtime-string->lisp result-ptr)))
  (test "PRINT-STRING" (string= result-str "\"hello\""))
  (format t "   Result: ~S~%" result-str))

;; Test 3: Print nil
(format t "~%3. Print NIL~%")
(format t "================~%")

(let* ((nil-value 0)
       (result-ptr (runtime-print-to-string nil-value))
       (result-str (runtime-string->lisp result-ptr)))
  (test "PRINT-NIL" (string= result-str "NIL"))
  (format t "   Result: ~S~%" result-str))

;; Test 4: Print simple list
(format t "~%4. Print Simple List~%")
(format t "====================~%")

(let* ((list-ptr (runtime-cons (ash 1 4)
                               (runtime-cons (ash 2 4)
                                           (runtime-cons (ash 3 4) 0))))
       (result-ptr (runtime-print-to-string list-ptr))
       (result-str (runtime-string->lisp result-ptr)))
  (test "PRINT-LIST" (string= result-str "(1 2 3)"))
  (format t "   Result: ~S~%" result-str))

;; Test 5: Read fixnum
(format t "~%5. Read Fixnum~%")
(format t "=================~%")

(handler-case
    (let* ((input-ptr (runtime-lisp->string "42"))
           (result (runtime-read-from-string input-ptr)))
      (test "READ-FIXNUM" (= result (ash 42 4)))
      (format t "   Result: ~X (tagged)~%" result))
  (error (e)
    (test "READ-FIXNUM" nil (format nil "~A" e))))

;; Test 6: Read string
(format t "~%6. Read String~%")
(format t "=================~%")

(handler-case
    (let* ((input-ptr (runtime-lisp->string "\"hello\""))
           (result (runtime-read-from-string input-ptr))
           (result-str (runtime-string->lisp result)))
      (test "READ-STRING" (string= result-str "hello"))
      (format t "   Result: ~S~%" result-str))
  (error (e)
    (test "READ-STRING" nil (format nil "~A" e))))

;; Test 7: Read simple list
(format t "~%7. Read Simple List~%")
(format t "===================~%")

(handler-case
    (let* ((input-ptr (runtime-lisp->string "(1 2 3)"))
           (result (runtime-read-from-string input-ptr)))
      ;; Check it's a cons
      (test "READ-LIST" (= (logand result #xF) +tag-cons+))
      (format t "   Result: ~X (cons)~%" result)
      ;; Print it back
      (let* ((print-result (runtime-print-to-string result))
             (print-str (runtime-string->lisp print-result)))
        (format t "   Printed: ~S~%" print-str)))
  (error (e)
    (test "READ-LIST" nil (format nil "~A" e))))

;; Test 8: Read quoted expression
(format t "~%8. Read Quoted Expression~%")
(format t "==========================~%")

(handler-case
    (let* ((input-ptr (runtime-lisp->string "'foo"))
           (result (runtime-read-from-string input-ptr)))
      ;; Should be (quote foo) which is a cons
      (test "READ-QUOTE" (= (logand result #xF) +tag-cons+))
      (let* ((print-result (runtime-print-to-string result))
             (print-str (runtime-string->lisp print-result)))
        (format t "   Printed: ~S~%" print-str)))
  (error (e)
    (test "READ-QUOTE" nil (format nil "~A" e))))

;; Test 9: Round-trip (read then print)
(format t "~%9. Round-trip Test~%")
(format t "===================~%")

(handler-case
    (let* ((input-ptr (runtime-lisp->string "(1 2 3)"))
           (read-result (runtime-read-from-string input-ptr))
           (print-result (runtime-print-to-string read-result))
           (final-str (runtime-string->lisp print-result)))
  (test "ROUND-TRIP" (string= final-str "(1 2 3)"))
  (format t "   Input:  \"(1 2 3)\"~%")
  (format t "   Output: ~S~%" final-str))
  (error (e)
    (test "ROUND-TRIP" nil (format nil "~A" e))))

;; Test 10: Print symbol in current package (no prefix)
(format t "~%10. Print Symbol Current Package~%")
(format t "===================================~%")

(handler-case
    (progn
      (runtime-make-package "PKG-A")
      (runtime-in-package "PKG-A")
      (let* ((sym (runtime-find-symbol "foo" "PKG-A"))
             (print-ptr (runtime-print-to-string sym))
             (print-str (runtime-string->lisp print-ptr)))
        (test "PRINT-SYMBOL-NO-PREFIX" (string= print-str "FOO"))
        (format t "   Printed: ~S~%" print-str)))
  (error (e)
    (test "PRINT-SYMBOL-NO-PREFIX" nil (format nil "~A" e))))

;; Test 11: Print symbol from another package (with prefix)
(format t "~%11. Print Symbol Other Package~%")
(format t "==================================~%")

(handler-case
    (progn
      (runtime-make-package "PKG-B")
      (runtime-in-package "PKG-B")
      (let* ((sym (runtime-find-symbol "bar" "PKG-A"))
             (print-ptr (runtime-print-to-string sym))
             (print-str (runtime-string->lisp print-ptr)))
        (test "PRINT-SYMBOL-PREFIX" (string= print-str "PKG-A::BAR"))
        (format t "   Printed: ~S~%" print-str)))
  (error (e)
    (test "PRINT-SYMBOL-PREFIX" nil (format nil "~A" e))))

;; Summary
(format t "~%=====================================~%")
(format t "  Test Results~%")
(format t "=====================================~%")
(format t "Total:  ~D~%" *test-count*)
(format t "Passed: ~D/~D~%~%" *pass-count* *test-count*)

(when (< *pass-count* *test-count*)
  (format t "Some tests failed!~%")
  (sb-ext:quit :unix-status 1))

(format t "All reader/printer tests passed!~%")

(sb-ext:quit)
