;;;; Habu Lisp Examples
;;;; Demonstrations of what you can do with Habu

;;;; ============================================
;;;; 1. CLASSIC ALGORITHMS
;;;; ============================================

;;; Factorial - n!
(defun factorial (n)
  (if (= n 0) 1
    (* n (factorial (- n 1)))))

;;; Fibonacci sequence
(defun fibonacci (n)
  (if (< n 2) n
    (+ (fibonacci (- n 1))
       (fibonacci (- n 2)))))

;;; Greatest Common Divisor (Euclid's algorithm)
(defun gcd (a b)
  (if (= b 0) a
    (gcd b (- a (* b (/ a b))))))

;;; Power function - base^exp
(defun power (base exp)
  (if (= exp 0) 1
    (* base (power base (- exp 1)))))

;;; Sum of squares
(defun sum-of-squares (a b)
  (+ (* a a) (* b b)))

;;;; ============================================
;;;; 2. LIST PROCESSING
;;;; ============================================

;;; Length of list
(defun length (lst)
  (if (= lst 0) 0
    (+ 1 (length (cdr lst)))))

;;; Sum all elements
(defun sum (lst)
  (if (= lst 0) 0
    (+ (car lst) (sum (cdr lst)))))

;;; Product of all elements
(defun product (lst)
  (if (= lst 0) 1
    (* (car lst) (product (cdr lst)))))

;;; Reverse a list
(defun reverse (lst)
  (reverse-helper lst 0))

(defun reverse-helper (lst acc)
  (if (= lst 0) acc
    (reverse-helper (cdr lst) (cons (car lst) acc))))

;;; Append two lists
(defun append (lst1 lst2)
  (if (= lst1 0) lst2
    (cons (car lst1) (append (cdr lst1) lst2))))

;;; Nth element of list (0-indexed)
(defun nth (n lst)
  (if (= n 0) (car lst)
    (nth (- n 1) (cdr lst))))

;;; Take first n elements
(defun take (n lst)
  (if (= n 0) 0
    (if (= lst 0) 0
      (cons (car lst) (take (- n 1) (cdr lst))))))

;;; Drop first n elements
(defun drop (n lst)
  (if (= n 0) lst
    (if (= lst 0) 0
      (drop (- n 1) (cdr lst)))))

;;;; ============================================
;;;; 3. HIGHER-ORDER FUNCTIONS
;;;; ============================================

;;; Map - apply function to each element
(defun map (f lst)
  (if (= lst 0) 0
    (cons (f (car lst)) (map f (cdr lst)))))

;;; Filter - select elements matching predicate
(defun filter (pred lst)
  (if (= lst 0) 0
    (if (pred (car lst))
      (cons (car lst) (filter pred (cdr lst)))
      (filter pred (cdr lst)))))

;;; Fold-left - accumulate from left
(defun fold (f init lst)
  (if (= lst 0) init
    (fold f (f init (car lst)) (cdr lst))))

;;; Reduce - fold without initial value
(defun reduce (f lst)
  (if (= lst 0) 0
    (if (= (cdr lst) 0) (car lst)
      (fold f (car lst) (cdr lst)))))

;;; Function composition
(defun compose (f g)
  (lambda (x) (f (g x))))

;;; Apply function twice
(defun twice (f)
  (lambda (x) (f (f x))))

;;;; ============================================
;;;; 4. LIST PREDICATES
;;;; ============================================

;;; Check if list contains element
(defun member? (x lst)
  (if (= lst 0) 0
    (if (= x (car lst)) 1
      (member? x (cdr lst)))))

;;; Check if all elements satisfy predicate
(defun all? (pred lst)
  (if (= lst 0) 1
    (if (pred (car lst))
      (all? pred (cdr lst))
      0)))

;;; Check if any element satisfies predicate
(defun any? (pred lst)
  (if (= lst 0) 0
    (if (pred (car lst)) 1
      (any? pred (cdr lst)))))

;;;; ============================================
;;;; 5. SORTING
;;;; ============================================

;;; Insertion sort
(defun insert (x lst)
  (if (= lst 0) (cons x 0)
    (if (< x (car lst))
      (cons x lst)
      (cons (car lst) (insert x (cdr lst))))))

(defun sort (lst)
  (if (= lst 0) 0
    (insert (car lst) (sort (cdr lst)))))

;;;; ============================================
;;;; 6. RANGE AND SEQUENCES
;;;; ============================================

;;; Generate range [start, end]
(defun range (start end)
  (if (> start end) 0
    (cons start (range (+ start 1) end))))

;;; Repeat element n times
(defun repeat (n x)
  (if (= n 0) 0
    (cons x (repeat (- n 1) x))))

;;; Generate list by applying function
(defun tabulate (n f)
  (tabulate-helper 0 n f))

(defun tabulate-helper (i n f)
  (if (>= i n) 0
    (cons (f i) (tabulate-helper (+ i 1) n f))))

;;;; ============================================
;;;; 7. NUMERIC UTILITIES
;;;; ============================================

;;; Absolute value
(defun abs (n)
  (if (< n 0) (- 0 n) n))

;;; Minimum of two numbers
(defun min (a b)
  (if (< a b) a b))

;;; Maximum of two numbers
(defun max (a b)
  (if (> a b) a b))

;;; Check if even
(defun even? (n)
  (= (- n (* 2 (/ n 2))) 0))

;;; Check if odd
(defun odd? (n)
  (if (even? n) 0 1))

;;; Check if positive
(defun positive? (n)
  (> n 0))

;;; Check if negative
(defun negative? (n)
  (< n 0))

;;; Check if zero
(defun zero? (n)
  (= n 0))

;;;; ============================================
;;;; 8. PRACTICAL EXAMPLES
;;;; ============================================

;;; Sum of list using fold
(defun sum-fold (lst)
  (fold + 0 lst))

;;; Count elements matching predicate
(defun count (pred lst)
  (fold (lambda (acc x) (if (pred x) (+ acc 1) acc)) 0 lst))

;;; Find maximum in list
(defun maximum (lst)
  (if (= lst 0) 0
    (if (= (cdr lst) 0) (car lst)
      (let ((rest-max (maximum (cdr lst))))
        (if (> (car lst) rest-max)
          (car lst)
          rest-max)))))

;;; Find minimum in list
(defun minimum (lst)
  (if (= lst 0) 0
    (if (= (cdr lst) 0) (car lst)
      (let ((rest-min (minimum (cdr lst))))
        (if (< (car lst) rest-min)
          (car lst)
          rest-min)))))

;;; Zip two lists into pairs
(defun zip (lst1 lst2)
  (if (= lst1 0) 0
    (if (= lst2 0) 0
      (cons (cons (car lst1) (car lst2))
            (zip (cdr lst1) (cdr lst2))))))

;;; Flatten one level of nesting
(defun flatten-one (lst)
  (if (= lst 0) 0
    (append (car lst) (flatten-one (cdr lst)))))

;;;; ============================================
;;;; 9. COMBINATORS AND FUNCTION UTILITIES
;;;; ============================================

;;; Identity function
(defun identity (x) x)

;;; Constant function
(defun const (x)
  (lambda (y) x))

;;; Flip function arguments
(defun flip (f)
  (lambda (x y) (f y x)))

;;; Curry binary function
(defun curry (f)
  (lambda (x)
    (lambda (y)
      (f x y))))

;;; Apply function to two arguments
(defun apply2 (f x y)
  (f x y))

;;;; ============================================
;;;; 10. EXAMPLE USAGE DEMONSTRATIONS
;;;; ============================================

;;; Example: Map square over list
(defun demo-map ()
  (let ((square (lambda (x) (* x x))))
    (map square '(1 2 3 4 5))))

;;; Example: Filter positive numbers
(defun demo-filter ()
  (filter positive? '(-2 -1 0 1 2 3)))

;;; Example: Sum with fold
(defun demo-fold ()
  (fold + 0 '(1 2 3 4 5 6 7 8 9 10)))

;;; Example: Function composition
(defun demo-compose ()
  (let ((double (lambda (x) (* 2 x))))
    (let ((add1 (lambda (x) (+ x 1))))
      (let ((f (compose double add1)))
        (f 5)))))

;;; Example: Twice combinator
(defun demo-twice ()
  (let ((add1 (lambda (x) (+ x 1))))
    (let ((add2 (twice add1)))
      (add2 10))))

;;; Example: Generate squares
(defun demo-squares ()
  (map (lambda (x) (* x x)) (range 1 10)))

;;; Example: Fibonacci sequence
(defun demo-fib-seq ()
  (map fibonacci (range 0 10)))

;;; Example: Sort a list
(defun demo-sort ()
  (sort '(5 2 8 1 9 3 7 4 6)))

;;; Example: Nested operations
(defun demo-nested ()
  (sum (map (lambda (x) (* x x)) (filter odd? (range 1 10)))))

;;;; ============================================
;;;; 11. PUZZLES AND FUN
;;;; ============================================

;;; Collatz conjecture step
(defun collatz (n)
  (if (even? n)
    (/ n 2)
    (+ (* 3 n) 1)))

;;; Number of Collatz steps to reach 1
(defun collatz-length (n)
  (collatz-length-helper n 0))

(defun collatz-length-helper (n count)
  (if (= n 1) count
    (collatz-length-helper (collatz n) (+ count 1))))

;;; Perfect number check (sum of divisors = number)
(defun divisors-sum (n)
  (divisors-sum-helper n 1 0))

(defun divisors-sum-helper (n i sum)
  (if (>= i n) sum
    (if (= (- n (* i (/ n i))) 0)
      (divisors-sum-helper n (+ i 1) (+ sum i))
      (divisors-sum-helper n (+ i 1) sum))))

(defun perfect? (n)
  (= (divisors-sum n) n))

;;; Digital root (repeatedly sum digits)
(defun digital-root (n)
  (if (< n 10) n
    (digital-root (digit-sum n))))

(defun digit-sum (n)
  (if (= n 0) 0
    (+ (- n (* 10 (/ n 10))) (digit-sum (/ n 10)))))

;;;; ============================================
;;;; USAGE INSTRUCTIONS
;;;; ============================================

;;; To use these examples, copy the functions you want into the REPL.
;;; Then call them:
;;;
;;; habu> (factorial 10)
;;; 3628800
;;;
;;; habu> (map (lambda (x) (* x x)) '(1 2 3 4 5))
;;; (1 4 9 16 25)
;;;
;;; habu> (filter positive? '(-2 -1 0 1 2))
;;; (1 2)
;;;
;;; habu> (fold + 0 (range 1 100))
;;; 5050
