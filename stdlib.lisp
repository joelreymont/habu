;;;; Habu Standard Library
;;;; Common utility functions for practical Lisp programming

;;; Boolean/Logic
(defun not (x) (if x nil 1))

(defun null? (x) (= x 0))

(defun pair? (x) (cons? x))

;;; Numeric predicates
(defun zero? (n) (= n 0))

(defun positive? (n) (> n 0))

(defun negative? (n) (< n 0))

(defun even? (n) (= (- n (* 2 (/ n 2))) 0))

(defun odd? (n) (not (even? n)))

;;; Numeric utilities
(defun abs (n) (if (< n 0) (- 0 n) n))

(defun min (a b) (if (< a b) a b))

(defun max (a b) (if (> a b) a b))

(defun square (x) (* x x))

(defun cube (x) (* x (* x x)))

;;; List utilities
(defun length (lst)
  (if (null? lst) 0
    (+ 1 (length (cdr lst)))))

(defun append (lst1 lst2)
  (if (null? lst1) lst2
    (cons (car lst1) (append (cdr lst1) lst2))))

(defun reverse (lst)
  (reverse-helper lst nil))

(defun reverse-helper (lst acc)
  (if (null? lst) acc
    (reverse-helper (cdr lst) (cons (car lst) acc))))

(defun nth (n lst)
  (if (= n 0) (car lst)
    (nth (- n 1) (cdr lst))))

(defun last (lst)
  (if (null? (cdr lst)) (car lst)
    (last (cdr lst))))

(defun take (n lst)
  (if (= n 0) nil
    (if (null? lst) nil
      (cons (car lst) (take (- n 1) (cdr lst))))))

(defun drop (n lst)
  (if (= n 0) lst
    (if (null? lst) nil
      (drop (- n 1) (cdr lst)))))

;;; Higher-order functions
(defun map (f lst)
  (if (null? lst) nil
    (cons (f (car lst)) (map f (cdr lst)))))

(defun filter (pred lst)
  (if (null? lst) nil
    (if (pred (car lst))
      (cons (car lst) (filter pred (cdr lst)))
      (filter pred (cdr lst)))))

(defun fold (f init lst)
  (if (null? lst) init
    (fold f (f init (car lst)) (cdr lst))))

(defun reduce (f lst)
  (if (null? lst) nil
    (if (null? (cdr lst)) (car lst)
      (fold f (car lst) (cdr lst)))))

;;; List predicates
(defun member? (x lst)
  (if (null? lst) nil
    (if (= x (car lst)) 1
      (member? x (cdr lst)))))

(defun all? (pred lst)
  (if (null? lst) 1
    (if (pred (car lst))
      (all? pred (cdr lst))
      nil)))

(defun any? (pred lst)
  (if (null? lst) nil
    (if (pred (car lst)) 1
      (any? pred (cdr lst)))))

;;; Numeric algorithms
(defun factorial (n)
  (if (= n 0) 1
    (* n (factorial (- n 1)))))

(defun fibonacci (n)
  (if (< n 2) n
    (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(defun gcd (a b)
  (if (= b 0) a
    (gcd b (- a (* b (/ a b))))))

(defun power (base exp)
  (if (= exp 0) 1
    (* base (power base (- exp 1)))))

;;; List construction
(defun range (start end)
  (if (> start end) nil
    (cons start (range (+ start 1) end))))

(defun repeat (n x)
  (if (= n 0) nil
    (cons x (repeat (- n 1) x))))

(defun replicate (n x) (repeat n x))

;;; List processing
(defun sum (lst)
  (fold + 0 lst))

(defun product (lst)
  (fold * 1 lst))

(defun count (pred lst)
  (fold (lambda (acc x) (if (pred x) (+ acc 1) acc)) 0 lst))

(defun zip (lst1 lst2)
  (if (null? lst1) nil
    (if (null? lst2) nil
      (cons (cons (car lst1) (car lst2))
            (zip (cdr lst1) (cdr lst2))))))

;;; Sorting (insertion sort)
(defun insert (x lst)
  (if (null? lst) (cons x nil)
    (if (< x (car lst))
      (cons x lst)
      (cons (car lst) (insert x (cdr lst))))))

(defun sort (lst)
  (if (null? lst) nil
    (insert (car lst) (sort (cdr lst)))))

;;; Functional composition
(defun compose (f g)
  (lambda (x) (f (g x))))

(defun twice (f)
  (lambda (x) (f (f x))))

(defun flip (f)
  (lambda (x y) (f y x)))

;;; Utility
(defun identity (x) x)

(defun const (x)
  (lambda (y) x))

(defun apply2 (f x y)
  (f x y))

;;; Examples and demonstrations
(defun example-map ()
  (map square '(1 2 3 4 5)))

(defun example-filter ()
  (filter positive? '(-2 -1 0 1 2)))

(defun example-fold ()
  (fold + 0 '(1 2 3 4 5)))

(defun example-compose ()
  (let ((double (lambda (x) (* 2 x))))
    (let ((add1 (lambda (x) (+ x 1))))
      (let ((f (compose double add1)))
        (f 5)))))
