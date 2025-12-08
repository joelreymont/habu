;; Test 1: Built-in string= (expected to FAIL with "unknown function")
(if (string= "test" "test") 42 99)
