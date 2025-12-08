;; Test 3: Symbol name vs literal
(let* ((sym (intern "hello"))
       (name (symbol-name sym)))
  (if (string= name "hello") 3 0))
