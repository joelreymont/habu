;; Define string= helpers in this file so they're available to h0-eval
(defun string=-loop (s1 s2 len i)
  (if (>= i len)
      t
      (if (= (string-ref s1 i) (string-ref s2 i))
          (string=-loop s1 s2 len (+ i 1))
          nil)))

(defun string= (s1 s2)
  (let ((len1 (string-length s1))
        (len2 (string-length s2)))
    (if (= len1 len2)
        (string=-loop s1 s2 len1 0)
        nil)))

;; Test 1: Direct string literals
(defun test1 ()
  (if (string= "abc" "abc") 1 0))

;; Test 2: Variables holding string literals
(defun test2 ()
  (let ((s1 "test")
        (s2 "test"))
    (if (string= s1 s2) 10 0)))

;; Test 3: Different strings
(defun test3 ()
  (if (string= "abc" "xyz") 0 100))

;; Test 4: Different lengths
(defun test4 ()
  (if (string= "ab" "abc") 0 1000))

;; Run all tests and sum results
;; Expected: 1 + 10 + 100 + 1000 = 1111
(+ (+ (test1) (test2)) (+ (test3) (test4)))
