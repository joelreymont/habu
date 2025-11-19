;;;; Test symbol system

(load "compiler.lisp")
(in-package :habu-compiler)

(initialize-runtime-integration)

(format t "~%Testing Symbol System~%")
(format t "====================~%~%")

(let ((intern-fn (find-symbol "RUNTIME-INTERN" :habu-runtime))
      (set-value-fn (find-symbol "SET-SYMBOL-VALUE" :habu-runtime))
      (get-value-fn (find-symbol "RUNTIME-SYMBOL-VALUE" :habu-runtime))
      (symbol-name-fn (find-symbol "RUNTIME-SYMBOL-NAME" :habu-runtime))
      (print-symbol-fn (find-symbol "PRINT-SYMBOL" :habu-runtime)))
  
  ;; Test interning
  (format t "1. Testing symbol interning~%")
  (let ((sym1 (funcall intern-fn "FOO"))
        (sym2 (funcall intern-fn "FOO"))
        (sym3 (funcall intern-fn "BAR")))
    (format t "  (intern \"FOO\") => ~X~%" sym1)
    (format t "  (intern \"FOO\") => ~X (same? ~A)~%" sym2 (if (= sym1 sym2) "YES" "NO"))
    (format t "  (intern \"BAR\") => ~X (different? ~A)~%~%" sym3 (if (/= sym1 sym3) "YES" "NO"))
    
    ;; Test symbol-value
    (format t "2. Testing symbol values~%")
    (funcall set-value-fn sym1 (ash 42 4))  ; Set FOO = 42 (as fixnum)
    (let ((value (funcall get-value-fn sym1)))
      (format t "  (set 'FOO 42) => ~X~%" value)
      (format t "  (symbol-value 'FOO) => ~D~%~%" (ash value -4)))
    
    ;; Test symbol names
    (format t "3. Testing symbol names~%")
    (format t "  (symbol-name ~X) => ~A~%" sym1 (funcall symbol-name-fn sym1))
    (format t "  (symbol-name ~X) => ~A~%~%" sym3 (funcall symbol-name-fn sym3))
    
    ;; Print full symbol info
    (format t "4. Symbol details~%")
    (funcall print-symbol-fn sym1)
    (format t "~%")
    
    (format t "✓ Symbol system tests complete!~%")))

(sb-ext:quit)
