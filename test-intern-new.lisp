;; Test: Intern the same symbol twice and check if they're eq
(defun test-intern ()
  (let ((sym1 (intern "FOO"))
        (sym2 (intern "FOO")))
    (if (eq sym1 sym2)
        42  ; GOOD: same symbol
        99))) ; BUG: different symbols

(test-intern)
