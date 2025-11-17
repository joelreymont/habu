;;;; Test numeric operators and predicates

(load "compiler.lisp")
(in-package :habu-compiler)

(format t "Testing numeric operators and predicates~%~%")

;;; Numeric operators
(format t "=== NUMERIC OPERATORS ===~%~%")

(format t "Testing: (min 5 10)~%")
(let ((code-x86 (compile-expression '(min 5 10) :arch :x86_64))
      (code-arm (compile-expression '(min 5 10) :arch :arm64)))
  (format t "  x86_64: ~D bytes, ARM64: ~D bytes~%" (length code-x86) (length code-arm)))

(format t "Testing: (max 5 10)~%")
(let ((code-x86 (compile-expression '(max 5 10) :arch :x86_64))
      (code-arm (compile-expression '(max 5 10) :arch :arm64)))
  (format t "  x86_64: ~D bytes, ARM64: ~D bytes~%" (length code-x86) (length code-arm)))

(format t "Testing: (abs -10)~%")
(let ((code-x86 (compile-expression '(abs -10) :arch :x86_64))
      (code-arm (compile-expression '(abs -10) :arch :arm64)))
  (format t "  x86_64: ~D bytes, ARM64: ~D bytes~%" (length code-x86) (length code-arm)))

(format t "Testing: (abs 10)~%")
(let ((code-x86 (compile-expression '(abs 10) :arch :x86_64))
      (code-arm (compile-expression '(abs 10) :arch :arm64)))
  (format t "  x86_64: ~D bytes, ARM64: ~D bytes~%" (length code-x86) (length code-arm)))

(format t "Testing: (1+ 5)~%")
(let ((code-x86 (compile-expression '(1+ 5) :arch :x86_64))
      (code-arm (compile-expression '(1+ 5) :arch :arm64)))
  (format t "  x86_64: ~D bytes, ARM64: ~D bytes~%" (length code-x86) (length code-arm)))

(format t "Testing: (1- 5)~%")
(let ((code-x86 (compile-expression '(1- 5) :arch :x86_64))
      (code-arm (compile-expression '(1- 5) :arch :arm64)))
  (format t "  x86_64: ~D bytes, ARM64: ~D bytes~%" (length code-x86) (length code-arm)))

;;; Predicates
(format t "~%=== PREDICATES ===~%~%")

(format t "Testing: (zerop 0)~%")
(let ((code-x86 (compile-expression '(zerop 0) :arch :x86_64))
      (code-arm (compile-expression '(zerop 0) :arch :arm64)))
  (format t "  x86_64: ~D bytes, ARM64: ~D bytes~%" (length code-x86) (length code-arm)))

(format t "Testing: (zerop 5)~%")
(let ((code-x86 (compile-expression '(zerop 5) :arch :x86_64))
      (code-arm (compile-expression '(zerop 5) :arch :arm64)))
  (format t "  x86_64: ~D bytes, ARM64: ~D bytes~%" (length code-x86) (length code-arm)))

(format t "Testing: (plusp 5)~%")
(let ((code-x86 (compile-expression '(plusp 5) :arch :x86_64))
      (code-arm (compile-expression '(plusp 5) :arch :arm64)))
  (format t "  x86_64: ~D bytes, ARM64: ~D bytes~%" (length code-x86) (length code-arm)))

(format t "Testing: (plusp -5)~%")
(let ((code-x86 (compile-expression '(plusp -5) :arch :x86_64))
      (code-arm (compile-expression '(plusp -5) :arch :arm64)))
  (format t "  x86_64: ~D bytes, ARM64: ~D bytes~%" (length code-x86) (length code-arm)))

(format t "Testing: (minusp -5)~%")
(let ((code-x86 (compile-expression '(minusp -5) :arch :x86_64))
      (code-arm (compile-expression '(minusp -5) :arch :arm64)))
  (format t "  x86_64: ~D bytes, ARM64: ~D bytes~%" (length code-x86) (length code-arm)))

(format t "Testing: (minusp 5)~%")
(let ((code-x86 (compile-expression '(minusp 5) :arch :x86_64))
      (code-arm (compile-expression '(minusp 5) :arch :arm64)))
  (format t "  x86_64: ~D bytes, ARM64: ~D bytes~%" (length code-x86) (length code-arm)))

(format t "Testing: (evenp 4)~%")
(let ((code-x86 (compile-expression '(evenp 4) :arch :x86_64))
      (code-arm (compile-expression '(evenp 4) :arch :arm64)))
  (format t "  x86_64: ~D bytes, ARM64: ~D bytes~%" (length code-x86) (length code-arm)))

(format t "Testing: (evenp 5)~%")
(let ((code-x86 (compile-expression '(evenp 5) :arch :x86_64))
      (code-arm (compile-expression '(evenp 5) :arch :arm64)))
  (format t "  x86_64: ~D bytes, ARM64: ~D bytes~%" (length code-x86) (length code-arm)))

(format t "Testing: (oddp 5)~%")
(let ((code-x86 (compile-expression '(oddp 5) :arch :x86_64))
      (code-arm (compile-expression '(oddp 5) :arch :arm64)))
  (format t "  x86_64: ~D bytes, ARM64: ~D bytes~%" (length code-x86) (length code-arm)))

(format t "Testing: (oddp 4)~%")
(let ((code-x86 (compile-expression '(oddp 4) :arch :x86_64))
      (code-arm (compile-expression '(oddp 4) :arch :arm64)))
  (format t "  x86_64: ~D bytes, ARM64: ~D bytes~%" (length code-x86) (length code-arm)))

;;; Combined tests
(format t "~%=== COMBINED TESTS ===~%~%")

(format t "Testing: (max (min 10 20) (abs -15))~%")
(let ((code-x86 (compile-expression '(max (min 10 20) (abs -15)) :arch :x86_64))
      (code-arm (compile-expression '(max (min 10 20) (abs -15)) :arch :arm64)))
  (format t "  x86_64: ~D bytes, ARM64: ~D bytes~%" (length code-x86) (length code-arm)))

(format t "Testing: (if (zerop (mod 10 2)) (evenp 10) (oddp 10))~%")
(let ((code-x86 (compile-expression '(if (zerop (mod 10 2)) (evenp 10) (oddp 10)) :arch :x86_64))
      (code-arm (compile-expression '(if (zerop (mod 10 2)) (evenp 10) (oddp 10)) :arch :arm64)))
  (format t "  x86_64: ~D bytes, ARM64: ~D bytes~%" (length code-x86) (length code-arm)))

(format t "Testing: (let ((x 5)) (and (plusp x) (oddp x)))~%")
(let ((code-x86 (compile-expression '(let ((x 5)) (and (plusp x) (oddp x))) :arch :x86_64))
      (code-arm (compile-expression '(let ((x 5)) (and (plusp x) (oddp x))) :arch :arm64)))
  (format t "  x86_64: ~D bytes, ARM64: ~D bytes~%" (length code-x86) (length code-arm)))

(format t "~%All numeric tests complete!~%")
