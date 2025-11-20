;;;; Test compile-to-arm64 function
;;;; Verify that it generates correct ARM64 bytes

(load "habu-arm64-codegen.lisp")

(defun print-bytes (bytes)
  "Print byte list in hex format"
  (if (cons? bytes)
    (progn
      (let ((b (car bytes)))
        (print b)
        (print " ")
        (print-bytes (cdr bytes))))
    nil))

(defun test-compile (name expr)
  (print name)
  (print ": ")
  (newline)
  (let ((bytes (compile-to-arm64 expr)))
    (print "Generated ")
    (print (length bytes))
    (print " bytes: ")
    (newline)
    (print-bytes bytes)
    (newline))
  (newline))

;;; Run tests
(print "Testing compile-to-arm64")
(newline)
(newline)

;;; Test 1: Literal 42
(test-compile "Literal 42" 42)

;;; Test 2: (+ 3 4)
(test-compile "(+ 3 4)" (quote (+ 3 4)))

;;; Test 3: (* 6 7)
(test-compile "(* 6 7)" (quote (* 6 7)))

;;; Test 4: (- 10 3)
(test-compile "(- 10 3)" (quote (- 10 3)))

;;; Test 5: Nested (+ (* 3 4) 5)
(test-compile "(+ (* 3 4) 5)" (quote (+ (* 3 4) 5)))

(print "Done")
(newline)
