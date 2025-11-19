;;;; Comparison Functions - Implemented in Habu Lisp
;;;; Uses string-ref and symbol-name primitives

(defun string-length (str)
  "Get length of a string (Habu string, not C string)"
  ;; For Habu strings, we need to use the raw length function
  ;; This is a helper that will need to be a primitive
  (string-length-raw str))

(defun string-compare-loop (s1 s2 idx len)
  "Compare two strings character by character"
  (if (>= idx len)
      (quote 1)  ; Reached end, strings are equal
      (if (= (string-ref s1 idx) (string-ref s2 idx))
          (string-compare-loop s1 s2 (+ idx (quote 1)) len)
          (quote nil))))  ; Characters differ

(defun string=? (s1 s2)
  "Check if two strings are equal"
  (if (string? s1)
      (if (string? s2)
          (let ((len1 (string-length s1)))
            (let ((len2 (string-length s2)))
              (if (= len1 len2)
                  (string-compare-loop s1 s2 (quote 0) len1)
                  (quote nil))))
          (quote nil))
      (quote nil)))

(defun symbol=? (sym1 sym2)
  "Check if two symbols are equal by comparing their names"
  (if (symbol? sym1)
      (if (symbol? sym2)
          (string=? (symbol-name sym1) (symbol-name sym2))
          (quote nil))
      (quote nil)))
