#x100
(let* ((sym (intern "hello"))
       (name (symbol-name sym)))
  (if (string= name "hello") 3 0))
