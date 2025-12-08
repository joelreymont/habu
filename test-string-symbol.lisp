;; Test 3: String from symbol-name vs literal
(let* ((sym (intern "hello"))
       (name (symbol-name sym))
       (result (string= name "hello")))
  (if result 1 0))
