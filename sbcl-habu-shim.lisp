;;;; SBCL shim for loading Habu code that expects Habu predicates/helpers

(defun nil? (x) (if x nil t))
(defun cons? (x) (consp x))
(defun symbol? (x) (symbolp x))
(defun symbol=? (a b) (eq a b))
(defun fixnum? (x) (typep x 'fixnum))
