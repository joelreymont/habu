;; Test SETQ implementation
;; Should return 2 after mutating x

(let ((x 1))
  (setq x 2)
  x)
