;;; Test string-append function

;; Test 1: Append two short strings
(let ((s1 "Hello")
      (s2 " World"))
  (let ((result (string-append s1 s2)))
    (sys-write 1 result (string-length result))
    (sys-write 1 "\n" 1)))

;; Test 2: Append empty string
(let ((s1 "Test")
      (s2 ""))
  (let ((result (string-append s1 s2)))
    (sys-write 1 result (string-length result))
    (sys-write 1 "\n" 1)))

;; Test 3: Check length
(let ((s1 "ABC")
      (s2 "DEF"))
  (let ((result (string-append s1 s2)))
    (if (= (string-length result) 6)
        (sys-exit 42)  ;; Success
        (sys-exit 1))))  ;; Failure
