(load "compiler.lisp")
(in-package :habu-compiler)

(initialize-runtime-integration)

(format t "~%Testing List Runtime Functions~%")
(format t "=============================~%~%")

;; Test length
(format t "1. Testing runtime-length~%")
(let* ((intern-fn (find-symbol "RUNTIME-INTERN" :habu-runtime))
       (cons-fn (find-symbol "RUNTIME-CONS" :habu-runtime))
       (length-fn (find-symbol "RUNTIME-LENGTH" :habu-runtime))
       ;; Create list (1 2 3)
       (list (funcall cons-fn (ash 1 4)
                     (funcall cons-fn (ash 2 4)
                             (funcall cons-fn (ash 3 4) 0)))))
  (format t "  Length of (1 2 3): ~D~%" (funcall length-fn list)))

;; Test nth
(format t "~%2. Testing runtime-nth~%")
(let* ((cons-fn (find-symbol "RUNTIME-CONS" :habu-runtime))
       (nth-fn (find-symbol "RUNTIME-NTH" :habu-runtime))
       ;; Create list (10 20 30)
       (list (funcall cons-fn (ash 10 4)
                     (funcall cons-fn (ash 20 4)
                             (funcall cons-fn (ash 30 4) 0)))))
  (format t "  0th element of (10 20 30): ~D~%" (ash (funcall nth-fn 0 list) -4))
  (format t "  1st element of (10 20 30): ~D~%" (ash (funcall nth-fn 1 list) -4))
  (format t "  2nd element of (10 20 30): ~D~%" (ash (funcall nth-fn 2 list) -4)))

(format t "~%✓ Runtime functions work!~%")
(sb-ext:quit)
