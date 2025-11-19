;;;; Simple ARM64 compilation test

(load "compiler.lisp")
(in-package :habu-compiler)

(initialize-runtime-integration)

(format t "~%Testing ARM64 Code Generation~%")
(format t "=============================~%~%")

;; Test 1: cons operation
(format t "[34m1. Test cons[0m~%")
(let ((code (compile-expression '(cons 10 20) :arch :arm64)))
  (format t "   ARM64 cons compiled: ~D bytes~%~%" (length code)))

;; Test 2: car operation
(format t "[34m2. Test car[0m~%")
(let ((code (compile-expression '(car (cons 10 20)) :arch :arm64)))
  (format t "   ARM64 car compiled: ~D bytes~%~%" (length code)))

;; Test 3: list operation
(format t "[34m3. Test list[0m~%")
(let ((code (compile-expression '(list 1 2 3) :arch :arm64)))
  (format t "   ARM64 list compiled: ~D bytes~%~%" (length code)))

;; Test 4: string-length operation
(format t "[34m4. Test string-length[0m~%")
(let ((code (compile-expression '(string-length "hello") :arch :arm64)))
  (format t "   ARM64 string-length compiled: ~D bytes~%~%" (length code)))

;; Test 5: read operation
(format t "[34m5. Test read[0m~%")
(let ((code (compile-expression '(read "(1 2 3)") :arch :arm64)))
  (format t "   ARM64 read compiled: ~D bytes~%~%" (length code)))

;; Test 6: file-open operation
(format t "[34m6. Test file-open[0m~%")
(let ((code (compile-expression '(file-open "/tmp/test.txt" "r") :arch :arm64)))
  (format t "   ARM64 file-open compiled: ~D bytes~%~%" (length code)))

(format t "[32mAll ARM64 compilation tests passed![0m~%")

(sb-ext:quit)
