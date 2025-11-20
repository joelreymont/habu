;;; Load the compiler
(load "habu-arm64-codegen.lisp")

;;; Helper to print bytes as C array
(defun print-bytes (bytes)
  (if (cons? bytes)
    (progn
      (print (car bytes))
      (if (cons? (cdr bytes))
        (progn
          (print ", ")
          (print-bytes (cdr bytes)))
        nil))
    nil))

(defun print-code-array (name expr)
  (print "/* ")
  (print name)
  (print " */\n")
  (print "unsigned char code[] = {")
  (print-bytes (compile-to-arm64 expr))
  (print "};\n\n"))

;;; Test 1: (cond ((> 5 3) 100))
(print-code-array "Test 1: (cond ((> 5 3) 100))"
                  (quote (cond ((> 5 3) 100))))

;;; Test 2: (cond ((< 5 3) 100) ((> 5 3) 200))
(print-code-array "Test 2: (cond ((< 5 3) 100) ((> 5 3) 200))"
                  (quote (cond ((< 5 3) 100) ((> 5 3) 200))))

;;; Test 3: Simple single clause true
(print-code-array "Test 3: (cond ((= 3 3) 42))"
                  (quote (cond ((= 3 3) 42))))

;;; Test 4: No matching clause - should return 0
(print-code-array "Test 4: (cond ((< 5 3) 100) ((< 2 1) 200))"
                  (quote (cond ((< 5 3) 100) ((< 2 1) 200))))
