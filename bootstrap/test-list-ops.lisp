;;;; Test new list operations: length, nth, append, reverse

(load "compiler.lisp")
(in-package :habu-compiler)

(initialize-runtime-integration)

(format t "~%Testing List Operations~%")
(format t "=======================~%~%")

;; Test 1: length
(format t "1. Testing length~%")
(let ((code1 (compile-expression '(length (list 1 2 3)) :arch :x86_64))
      (code2 (compile-expression '(length (list)) :arch :x86_64))
      (code3 (compile-expression '(length (list 10 20 30 40 50)) :arch :x86_64)))
  (format t "  (length (list 1 2 3)) compiles to ~D bytes~%" (length code1))
  (format t "  (length (list)) compiles to ~D bytes~%" (length code2))
  (format t "  (length (list 10 20 30 40 50)) compiles to ~D bytes~%~%" (length code3)))

;; Test 2: nth
(format t "2. Testing nth~%")
(let ((code1 (compile-expression '(nth 0 (list 10 20 30)) :arch :x86_64))
      (code2 (compile-expression '(nth 1 (list 10 20 30)) :arch :x86_64))
      (code3 (compile-expression '(nth 2 (list 10 20 30)) :arch :x86_64)))
  (format t "  (nth 0 (list 10 20 30)) compiles to ~D bytes~%" (length code1))
  (format t "  (nth 1 (list 10 20 30)) compiles to ~D bytes~%" (length code2))
  (format t "  (nth 2 (list 10 20 30)) compiles to ~D bytes~%~%" (length code3)))

;; Test 3: append
(format t "3. Testing append~%")
(let ((code1 (compile-expression '(append (list 1 2) (list 3 4)) :arch :x86_64))
      (code2 (compile-expression '(append (list) (list 1 2 3)) :arch :x86_64))
      (code3 (compile-expression '(append (list 1 2 3) (list)) :arch :x86_64)))
  (format t "  (append (list 1 2) (list 3 4)) compiles to ~D bytes~%" (length code1))
  (format t "  (append (list) (list 1 2 3)) compiles to ~D bytes~%" (length code2))
  (format t "  (append (list 1 2 3) (list)) compiles to ~D bytes~%~%" (length code3)))

;; Test 4: reverse
(format t "4. Testing reverse~%")
(let ((code1 (compile-expression '(reverse (list 1 2 3)) :arch :x86_64))
      (code2 (compile-expression '(reverse (list)) :arch :x86_64))
      (code3 (compile-expression '(reverse (list 5 4 3 2 1)) :arch :x86_64)))
  (format t "  (reverse (list 1 2 3)) compiles to ~D bytes~%" (length code1))
  (format t "  (reverse (list)) compiles to ~D bytes~%" (length code2))
  (format t "  (reverse (list 5 4 3 2 1)) compiles to ~D bytes~%~%" (length code3)))

;; Test 5: Combined operations
(format t "5. Testing combined operations~%")
(let ((code1 (compile-expression '(length (append (list 1 2) (list 3 4 5))) :arch :x86_64))
      (code2 (compile-expression '(nth 0 (reverse (list 1 2 3))) :arch :x86_64))
      (code3 (compile-expression '(length (reverse (list 10 20 30))) :arch :x86_64)))
  (format t "  (length (append (list 1 2) (list 3 4 5))) compiles to ~D bytes~%" (length code1))
  (format t "  (nth 0 (reverse (list 1 2 3))) compiles to ~D bytes~%" (length code2))
  (format t "  (length (reverse (list 10 20 30))) compiles to ~D bytes~%~%" (length code3)))

(format t "✓ All list operations compile successfully!~%")
(sb-ext:quit)
