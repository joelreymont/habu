;;; Bootstrap Test Program
;;; This program tests the core compiler without defmacro

;; Recursive factorial
(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (1- n)))))

;; Higher-order function
(defun map1 (f lst)
  (if (null lst)
      nil
      (cons (funcall f (car lst))
            (map1 f (cdr lst)))))

;; Square function
(defun square (x) (* x x))

;; Sum a list using reduce-like pattern
(defun sum-list (lst)
  (if (null lst)
      0
      (+ (car lst) (sum-list (cdr lst)))))

;; Test everything
(defun main ()
  (let* ((fact-result (factorial 5))          ; Should be 120
         (squares (map1 #'square '(1 2 3 4))) ; Should be (1 4 9 16)
         (sum (sum-list squares)))            ; Should be 30
    (println fact-result)
    (println sum)
    (+ fact-result sum)))  ; Should return 150

(main)
