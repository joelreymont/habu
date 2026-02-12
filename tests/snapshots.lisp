;;;; Habu Snapshot Tests
;;;; Each test is: ;;; TEST: <description>
;;;;                <expression>
;;;;                ;;; => <expected output>
;;;;
;;;; Run with: tools/snapshot-test

;;; ============================================================================
;;; Arithmetic
;;; ============================================================================

;;; TEST: integer addition
(+ 1 2)
;;; => 3

;;; TEST: integer subtraction
(- 10 3)
;;; => 7

;;; TEST: integer multiplication
(* 6 7)
;;; => 42

;;; TEST: integer division
(/ 10 2)
;;; => 5

;;; TEST: nested arithmetic
(+ (* 3 4) (- 10 5))
;;; => 17

;;; TEST: negation
(- 42)
;;; => -42

;;; TEST: zero
(+ 0 0)
;;; => 0

;;; TEST: large fixnum
(+ 1000000000 2000000000)
;;; => 3000000000

;;; TEST: mod positive
(mod 17 5)
;;; => 2

;;; TEST: rem positive
(rem 17 5)
;;; => 2

;;; TEST: mod negative dividend
(mod -7 2)
;;; => 1

;;; TEST: rem negative dividend
(rem -7 2)
;;; => -1

;;; TEST: abs positive
(abs 42)
;;; => 42

;;; TEST: abs negative
(abs -42)
;;; => 42

;;; TEST: min
(min 3 1 4 1 5)
;;; => 1

;;; TEST: max
(max 3 1 4 1 5)
;;; => 5

;;; TEST: evenp
(evenp 4)
;;; => t

;;; TEST: oddp
(oddp 3)
;;; => t

;;; TEST: zerop
(zerop 0)
;;; => t

;;; TEST: gcd
(gcd 12 8)
;;; => 4

;;; TEST: lcm
(lcm 4 6)
;;; => 12

;;; TEST: logand
(logand #xff #x0f)
;;; => 15

;;; TEST: logior
(logior #xf0 #x0f)
;;; => 255

;;; TEST: logxor
(logxor #xff #x0f)
;;; => 240

;;; TEST: ash left
(ash 1 4)
;;; => 16

;;; TEST: ash right
(ash 16 -4)
;;; => 1

;;; ============================================================================
;;; Comparisons
;;; ============================================================================

;;; TEST: numeric equal
(= 3 3)
;;; => t

;;; TEST: numeric not equal
(= 3 4)
;;; => nil

;;; TEST: less than
(< 1 2)
;;; => t

;;; TEST: greater than
(> 2 1)
;;; => t

;;; TEST: less than or equal
(<= 2 2)
;;; => t

;;; TEST: greater than or equal
(>= 3 2)
;;; => t

;;; ============================================================================
;;; Cons / Lists
;;; ============================================================================

;;; TEST: cons
(cons 1 2)
;;; => (1 . 2)

;;; TEST: car
(car '(1 2 3))
;;; => 1

;;; TEST: cdr
(cdr '(1 2 3))
;;; => (2 3)

;;; TEST: list
(list 1 2 3)
;;; => (1 2 3)

;;; TEST: list length
(length '(a b c d))
;;; => 4

;;; TEST: null empty
(null '())
;;; => t

;;; TEST: null non-empty
(null '(1))
;;; => nil

;;; TEST: nth
(nth 2 '(a b c d))
;;; => C

;;; TEST: nthcdr
(nthcdr 2 '(a b c d))
;;; => (C D)

;;; TEST: last
(last '(1 2 3))
;;; => (3)

;;; TEST: append
(append '(1 2) '(3 4))
;;; => (1 2 3 4)

;;; TEST: reverse
(reverse '(1 2 3))
;;; => (3 2 1)

;;; TEST: member
(member 3 '(1 2 3 4 5))
;;; => (3 4 5)

;;; TEST: assoc
(assoc 'b '((a . 1) (b . 2) (c . 3)))
;;; => (B . 2)

;;; TEST: mapcar
(mapcar #'1+ '(1 2 3))
;;; => (2 3 4)

;;; TEST: remove-if
(remove-if #'evenp '(1 2 3 4 5))
;;; => (1 3 5)

;;; TEST: reduce
(reduce #'+ '(1 2 3 4 5))
;;; => 15

;;; TEST: every true
(every #'numberp '(1 2 3))
;;; => t

;;; TEST: every false
(every #'numberp '(1 "a" 3))
;;; => nil

;;; TEST: some true
(some #'evenp '(1 2 3))
;;; => t

;;; TEST: some false
(some #'evenp '(1 3 5))
;;; => nil

;;; TEST: copy-list
(let ((x '(1 2 3))) (eq x (copy-list x)))
;;; => nil

;;; TEST: subseq string mid
(subseq "abcde" 1 3)
;;; => "bc"

;;; TEST: position
(position 3 '(1 2 3 4))
;;; => 2

;;; TEST: find
(find 3 '(1 2 3 4))
;;; => 3

;;; TEST: count
(count 3 '(1 3 2 3 4 3))
;;; => 3

;;; TEST: remove
(remove 3 '(1 3 2 3 4))
;;; => (1 2 4)

;;; ============================================================================
;;; Strings
;;; ============================================================================

;;; TEST: string length
(length "hello")
;;; => 5

;;; TEST: string concatenate
(concatenate 'string "hello" " " "world")
;;; => "hello world"

;;; TEST: string-upcase
(string-upcase "hello")
;;; => "HELLO"

;;; TEST: string-downcase
(string-downcase "HELLO")
;;; => "hello"

;;; TEST: subseq string
(subseq "hello world" 6)
;;; => "world"

;;; TEST: string equal
(string= "abc" "abc")
;;; => t

;;; TEST: string not equal
(string= "abc" "def")
;;; => nil

;;; TEST: char-code
(char-code #\A)
;;; => 65

;;; TEST: string-trim
(string-trim " " "  hello  ")
;;; => "hello"

;;; ============================================================================
;;; Vectors / Arrays
;;; ============================================================================

;;; TEST: vector literal
(vector 1 2 3)
;;; => #(1 2 3)

;;; TEST: make-array via vector
(let ((v (make-array 3 :initial-element 0))) (aref v 0))
;;; => 0

;;; TEST: aref
(aref (vector 10 20 30) 1)
;;; => 20

;;; TEST: vector length
(length (vector 1 2 3 4))
;;; => 4

;;; ============================================================================
;;; Hash Tables
;;; ============================================================================

;;; TEST: hash table put/get
(let ((h (make-hash-table)))
  (setf (gethash 'a h) 1)
  (setf (gethash 'b h) 2)
  (gethash 'a h))
;;; => 1

;;; TEST: hash table missing key
(let ((h (make-hash-table)))
  (gethash 'missing h))
;;; => nil

;;; TEST: hash-table-count
(let ((h (make-hash-table)))
  (setf (gethash 'a h) 1)
  (setf (gethash 'b h) 2)
  (hash-table-count h))
;;; => 2

;;; TEST: remhash
(let ((h (make-hash-table)))
  (setf (gethash 'a h) 1)
  (remhash 'a h)
  (gethash 'a h))
;;; => nil

;;; ============================================================================
;;; Control Flow
;;; ============================================================================

;;; TEST: if true
(if t 1 2)
;;; => 1

;;; TEST: if false
(if nil 1 2)
;;; => 2

;;; TEST: when true
(when t 42)
;;; => 42

;;; TEST: when false
(when nil 42)
;;; => nil

;;; TEST: unless true
(unless t 42)
;;; => nil

;;; TEST: unless false
(unless nil 42)
;;; => 42

;;; TEST: cond
(cond ((= 1 2) 'a) ((= 1 1) 'b) (t 'c))
;;; => B

;;; TEST: case
(case 2 (1 'one) (2 'two) (3 'three))
;;; => TWO

;;; TEST: and true
(and 1 2 3)
;;; => 3

;;; TEST: and false
(and 1 nil 3)
;;; => nil

;;; TEST: or first
(or nil nil 3)
;;; => 3

;;; TEST: or nil
(or nil nil nil)
;;; => nil

;;; TEST: not true
(not t)
;;; => nil

;;; TEST: not false
(not nil)
;;; => t

;;; TEST: progn
(progn 1 2 3)
;;; => 3

;;; ============================================================================
;;; Let / Variables
;;; ============================================================================

;;; TEST: let basic
(let ((x 10) (y 20)) (+ x y))
;;; => 30

;;; TEST: let* sequential
(let* ((x 10) (y (* x 2))) y)
;;; => 20

;;; TEST: setq
(let ((x 0)) (setq x 42) x)
;;; => 42

;;; ============================================================================
;;; Functions / Closures
;;; ============================================================================

;;; TEST: defun and call
(progn (defun double (x) (* x 2)) (double 21))
;;; => 42

;;; TEST: lambda
(funcall (lambda (x) (* x 3)) 14)
;;; => 42

;;; TEST: closure capture
(let ((n 10)) (funcall (lambda (x) (+ x n)) 32))
;;; => 42

;;; TEST: recursive fib
(progn (defun fib (n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 10))
;;; => 55

;;; TEST: optional args
(progn (defun opt-test (x &optional (y 10)) (+ x y)) (list (opt-test 1) (opt-test 1 2)))
;;; => (11 3)

;;; TEST: rest args
(progn (defun rest-test (x &rest ys) (cons x ys)) (rest-test 1 2 3))
;;; => (1 2 3)

;;; TEST: keyword args
(progn (defun kw-test (&key (x 0) (y 0)) (list x y)) (kw-test :y 5 :x 3))
;;; => (3 5)

;;; TEST: apply
(apply #'+ '(1 2 3))
;;; => 6

;;; TEST: funcall
(funcall #'+ 1 2 3)
;;; => 6

;;; ============================================================================
;;; Multiple Values
;;; ============================================================================

;;; TEST: values basic
(multiple-value-list (values 1 2 3))
;;; => (1 2 3)

;;; TEST: values empty
(multiple-value-list (values))
;;; => (nil)

;;; TEST: multiple-value-bind
(multiple-value-bind (a b) (values 10 20) (+ a b))
;;; => 30

;;; TEST: mv through if then-branch
(multiple-value-list (if t (values 1 2 3) nil))
;;; => (1 2 3)

;;; TEST: mv through if else-branch
(multiple-value-list (if nil (values 1 2) (values 3 4)))
;;; => (3 4)

;;; TEST: mv from function call
(progn (defun ret-mv () (values 10 20)) (multiple-value-bind (a b) (ret-mv) (list a b)))
;;; => (10 20)

;;; TEST: mv from function with if
(progn
  (defun mv-if (x) (if (> x 0) (values x 1) (values x -1)))
  (multiple-value-bind (a b) (mv-if 5) (list a b)))
;;; => (5 1)

;;; TEST: mv-bind doesn't clobber params
(progn
  (defun mvb-params (x y)
    (multiple-value-bind (a b) (values 10 20)
      (list x y a b)))
  (mvb-params 1 2))
;;; => (1 2 10 20)

;;; TEST: mv-bind after let
(let ((x 100))
  (multiple-value-bind (a b) (values 1 2)
    (list x a b)))
;;; => (100 1 2)

;;; TEST: floor 1-arg float
(multiple-value-list (floor 3.7))
;;; => (3 0.6999999999999993)

;;; TEST: floor 2-arg
(multiple-value-list (floor 17 5))
;;; => (3 2)

;;; TEST: floor negative
(multiple-value-list (floor -7 2))
;;; => (-4 1)

;;; TEST: truncate 2-arg
(multiple-value-list (truncate 7 2))
;;; => (3 1)

;;; TEST: ceiling 2-arg
(multiple-value-list (ceiling 7 2))
;;; => (4 -1)

;;; TEST: round to even 2.5
(multiple-value-bind (q r) (round 2.5) q)
;;; => 2

;;; TEST: round to even 3.5
(multiple-value-bind (q r) (round 3.5) q)
;;; => 4

;;; TEST: floor 1-arg integer identity
(multiple-value-list (floor 10))
;;; => (10 0)

;;; ============================================================================
;;; Type Predicates
;;; ============================================================================

;;; TEST: numberp
(numberp 42)
;;; => t

;;; TEST: integerp
(integerp 42)
;;; => t

;;; TEST: floatp
(floatp 3.14)
;;; => t

;;; TEST: stringp
(stringp "hello")
;;; => t

;;; TEST: symbolp
(symbolp 'foo)
;;; => t

;;; TEST: consp
(consp '(1 2))
;;; => t

;;; TEST: listp nil
(listp nil)
;;; => t

;;; TEST: listp cons
(listp '(1 2))
;;; => t

;;; TEST: atom number
(atom 42)
;;; => t

;;; TEST: atom cons
(atom '(1 2))
;;; => nil

;;; TEST: characterp
(characterp #\a)
;;; => t

;;; TEST: keywordp
(keywordp :foo)
;;; => t

;;; ============================================================================
;;; Error Handling
;;; ============================================================================

;;; TEST: handler-case catches error
(handler-case (error "boom") (error (e) :caught))
;;; => :CAUGHT

;;; TEST: handler-case type-error
(handler-case (+ 1 "a") (type-error (e) :type-err))
;;; => :TYPE-ERR

;;; TEST: handler-case division-by-zero
(handler-case (/ 1 0) (division-by-zero (e) :div-zero))
;;; => :DIV-ZERO

;;; TEST: handler-case no match falls through
(handler-case (values 42) (error (e) :never))
;;; => 42

;;; ============================================================================
;;; LOOP
;;; ============================================================================

;;; TEST: loop collect
(loop for i from 1 to 5 collect i)
;;; => (1 2 3 4 5)

;;; TEST: loop sum
(loop for i from 1 to 10 sum i)
;;; => 55

;;; TEST: loop count
(loop for i from 1 to 10 count (evenp i))
;;; => 5

;;; TEST: loop when
(loop for i from 1 to 10 when (oddp i) collect i)
;;; => (1 3 5 7 9)

;;; TEST: loop for on
(loop for x on '(a b c) collect (car x))
;;; => (A B C)

;;; TEST: loop for in
(loop for x in '(1 2 3) collect (* x x))
;;; => (1 4 9)

;;; TEST: loop across vector
(loop for x across #(10 20 30) sum x)
;;; => 60

;;; TEST: loop maximize
(loop for i in '(3 1 4 1 5 9 2 6) maximize i)
;;; => 9

;;; TEST: loop minimize
(loop for i in '(3 1 4 1 5 9 2 6) minimize i)
;;; => 1

;;; TEST: loop do
(let ((sum 0)) (loop for i from 1 to 5 do (setq sum (+ sum i))) sum)
;;; => 15

;;; TEST: loop with
(loop with x = 10 for i from 1 to 3 collect (+ x i))
;;; => (11 12 13)

;;; TEST: dotimes
(let ((acc 0)) (dotimes (i 5 acc) (setq acc (+ acc i))))
;;; => 10

;;; TEST: dolist
(let ((acc nil)) (dolist (x '(1 2 3) (reverse acc)) (push (* x x) acc)))
;;; => (1 4 9)

;;; ============================================================================
;;; Format
;;; ============================================================================

;;; TEST: format integer
(format nil "~D" 42)
;;; => "42"

;;; TEST: format string
(format nil "~A" "hello")
;;; => "hello"

;;; TEST: format multiple
(format nil "~A + ~A = ~A" 1 2 3)
;;; => "1 + 2 = 3"

;;; TEST: format tilde-A
(format nil "value=~A" 42)
;;; => "value=42"

;;; ============================================================================
;;; CLOS basics
;;; ============================================================================

;;; TEST: defclass and make-instance
(progn
  (defclass point () ((x :initarg :x :accessor point-x) (y :initarg :y :accessor point-y)))
  (let ((p (make-instance 'point :x 3 :y 4)))
    (list (point-x p) (point-y p))))
;;; => (3 4)

;;; TEST: defmethod
(progn
  (defclass animal () ((name :initarg :name :accessor animal-name)))
  (defmethod speak ((a animal)) (format nil "~A says hello" (animal-name a)))
  (speak (make-instance 'animal :name "Dog")))
;;; => "Dog says hello"

;;; ============================================================================
;;; Packages
;;; ============================================================================

;;; TEST: find-package
(not (null (find-package "CL")))
;;; => t

;;; TEST: package-name
(package-name (find-package "CL"))
;;; => "COMMON-LISP"

;;; ============================================================================
;;; Misc
;;; ============================================================================

;;; TEST: identity
(identity 42)
;;; => 42

;;; TEST: constantly
(funcall (constantly 42) 1 2 3)
;;; => 42

;;; TEST: complement
(funcall (complement #'evenp) 3)
;;; => t

;;; TEST: typep integer
(typep 42 'integer)
;;; => t

;;; TEST: typep string
(typep "hello" 'string)
;;; => t

;;; TEST: coerce list to vector
(coerce '(1 2 3) 'vector)
;;; => #(1 2 3)

;;; TEST: sort
(sort (list 3 1 4 1 5) #'<)
;;; => (1 1 3 4 5)

;;; TEST: stable-sort
(stable-sort (list 3 1 4 1 5) #'<)
;;; => (1 1 3 4 5)

;;; TEST: write-to-string
(write-to-string 42)
;;; => "42"

;;; TEST: multiple-value-prog1
(multiple-value-list (multiple-value-prog1 (values 1 2 3) (values 4 5)))
;;; => (1 2 3)

;;; TEST: prog1
(prog1 1 2 3)
;;; => 1

;;; TEST: prog2
(prog2 1 2 3)
;;; => 2

;;; ============================================================
;;; Additional CL Compliance Tests
;;; ============================================================

;;; TEST: /= not-equal
(/= 1 2)
;;; => t

;;; TEST: /= equal
(/= 1 1)
;;; => nil

;;; TEST: char returns character
(characterp (char "hello" 0))
;;; => t

;;; TEST: char-code of char
(char-code (char "hello" 0))
;;; => 104

;;; TEST: char= comparison
(char= (char "abc" 0) #\a)
;;; => t

;;; TEST: coerce integer to float
(floatp (coerce 3 'float))
;;; => t

;;; TEST: with-output-to-string
(with-output-to-string (s) (princ "hello" s))
;;; => "hello"

;;; TEST: with-output-to-string format
(with-output-to-string (s) (princ 42 s) (princ " " s) (princ "world" s))
;;; => "42 world"

;;; TEST: format ~R cardinal
(format nil "~R" 42)
;;; => "forty-two"

;;; TEST: format ~:R ordinal
(format nil "~:R" 3)
;;; => "3rd"

;;; TEST: format ~F float
(format nil "~F" 3.14)
;;; => "3.14"

;;; TEST: multiple-value-setq
(let ((a 0) (b 0)) (multiple-value-setq (a b) (values 3 4)) (list a b))
;;; => (3 4)

;;; TEST: assoc-if
(assoc-if (lambda (k) (> k 1)) '((1 . :a) (2 . :b)))
;;; => (2 . :B)

;;; TEST: string-trim with char
(string-trim " " "  hello  ")
;;; => "hello"

;;; TEST: isqrt
(isqrt 25)
;;; => 5

;;; TEST: isqrt 0
(isqrt 0)
;;; => 0

;;; TEST: restart system
(handler-bind ((error (lambda (c) (invoke-restart 'use-value 42)))) (restart-case (error "e") (use-value (v) v)))
;;; => 42

;;; TEST: eval-when execute
(eval-when (:execute) (+ 1 2))
;;; => 3

;;; TEST: rational division reduces
(/ 10 2)
;;; => 5

;;; TEST: rational non-reducible
(/ 1 3)
;;; => 1/3

;;; TEST: float printing with decimal
3.0
;;; => 3.0

;;; TEST: float arithmetic preserves type
(+ 3 0.0)
;;; => 3.0

;;; TEST: float function
(float 42)
;;; => 42.0

;;; TEST: write-to-string with float
(write-to-string 3.0)
;;; => "3.0"

;;; TEST: typecase integer
(typecase 42 (string :string) (integer :integer) (t :other))
;;; => :INTEGER

;;; TEST: handler-bind with restart
(handler-bind ((error (lambda (c) (invoke-restart 'use-value 0)))) (restart-case (/ 1 0) (use-value (v) v)))
;;; => 0

;;; TEST: every
(every #'numberp '(1 2 3))
;;; => t

;;; TEST: some
(some #'symbolp '(1 :a 2))
;;; => t

;;; TEST: loop across string
(loop for c across "abc" collect c)
;;; => (#\a #\b #\c)

;;; TEST: format conditional
(format nil "~[zero~;one~;two~]" 1)
;;; => "one"

;;; TEST: search in string
(search "ll" "hello")
;;; => 2

;;; TEST: string from symbol
(string 'hello)
;;; => "HELLO"

;;; TEST: eql specializer method
(progn (defgeneric eql-test (x)) (defmethod eql-test ((x (eql 0))) :zero) (defmethod eql-test ((x integer)) :other) (eql-test 0))
;;; => :ZERO

;;; TEST: abs float
(abs -3.14)
;;; => 3.139999999999997

;;; TEST: abs fixnum
(abs -42)
;;; => 42

;;; TEST: string-upcase symbol
(string-upcase 'hello)
;;; => "HELLO"

;;; TEST: string-downcase symbol
(string-downcase 'HELLO)
;;; => "hello"

;;; TEST: format ~10A padding
(format nil "~10A" "hi")
;;; => "hi        "

;;; TEST: format ~5D padding
(format nil "~5D" 42)
;;; => "   42"

;;; TEST: read-from-string multiple values
(multiple-value-list (read-from-string "123 456"))
;;; => (123 4)

;;; TEST: read-from-string list
(read-from-string "(+ 1 2)")
;;; => (+ 1 2)

;;; TEST: ldb byte
(ldb (byte 8 0) 255)
;;; => 255

;;; TEST: dpb
(dpb 1 (byte 1 0) 0)
;;; => 1

;;; TEST: complexp
(complexp #C(1 2))
;;; => t

;;; TEST: realpart
(realpart #C(3 4))
;;; => 3.0

;;; TEST: with-accessors CLOS
(progn (defclass pt5 () ((x :initarg :x :accessor pt5-x))) (let ((p (make-instance 'pt5 :x 10))) (with-accessors ((px pt5-x)) p px)))
;;; => 10

;;; TEST: around method
(progn (defgeneric g6 (x)) (defmethod g6 ((x string)) x) (defmethod g6 :around ((x string)) (format nil "[~A]" (call-next-method))) (g6 "hi"))
;;; => "[hi]"

;;; TEST: last with n
(last '(1 2 3 4) 2)
;;; => (3 4)

;;; TEST: butlast
(butlast '(1 2 3 4))
;;; => (1 2 3)

;;; TEST: concatenate list
(concatenate 'list '(1 2) '(3 4))
;;; => (1 2 3 4)

;;; TEST: concatenate vector
(concatenate 'vector '(1 2) '(3 4))
;;; => #(1 2 3 4)

;;; TEST: princ-to-string
(princ-to-string 42)
;;; => "42"

;;; TEST: prin1-to-string
(prin1-to-string "hello")
;;; => ""hello""

;;; TEST: digit-char
(digit-char 3)
;;; => #\3

;;; TEST: char-name
(char-name #\Space)
;;; => "Space"

;;; TEST: name-char
(name-char "Space")
;;; => #\space

;;; TEST: stable-sort
(stable-sort (list 3 1 2) #'<)
;;; => (1 2 3)

;;; TEST: mapcan
(mapcan (lambda (x) (list x (* x x))) '(1 2 3))
;;; => (1 1 2 4 3 9)

;;; TEST: nreverse
(nreverse (list 1 2 3))
;;; => (3 2 1)

;;; TEST: intersection
(intersection '(1 2 3 4) '(2 4 6))
;;; => (2 4)

;;; TEST: set-difference
(sort (set-difference '(1 2 3 4) '(2 4)) #'<)
;;; => (1 3)

;;; TEST: string-left-trim
(string-left-trim " " "  hello  ")
;;; => "hello  "

;;; TEST: string-right-trim
(string-right-trim " " "  hello  ")
;;; => "  hello"

;;; TEST: char-equal case-insensitive
(char-equal #\a #\A)
;;; => t

;;; TEST: string-equal case-insensitive
(string-equal "Hello" "hello")
;;; => t

;;; TEST: with-open-file
(with-open-file (in "lib/stdlib.habu" :direction :input) (read-line in))
;;; => "; Habu Standard Library"

;;; TEST: probe-file exists
(if (probe-file "lib/stdlib.habu") :found :missing)
;;; => :FOUND

;;; TEST: typep comprehensive
(list (typep 42 'integer) (typep "hi" 'string) (typep '(1) 'list) (typep 3.14 'float))
;;; => (t t t t)

;;; TEST: = rational comparison
(= (/ 1 3) 1/3)
;;; => t

;;; TEST: = float-integer comparison
(= 3 3.0)
;;; => t

;;; TEST: = complex comparison
(= #C(1 2) #C(1 2))
;;; => t

;;; TEST: < rational
(< 1/3 1/2)
;;; => t

;;; TEST: > rational
(> 3/4 1/2)
;;; => t

;;; TEST: <= rational
(<= 1/2 1/2)
;;; => t

;;; TEST: < mixed fixnum/rational
(< 1/3 1)
;;; => t

;;; TEST: > mixed fixnum/rational
(> 2 3/2)
;;; => t

;;; TEST: parse-integer basic
(parse-integer "42")
;;; => 42

;;; TEST: parse-integer negative
(parse-integer "-42")
;;; => -42

;;; TEST: parse-integer with whitespace
(parse-integer "  42  ")
;;; => 42

;;; TEST: parse-integer hex
(parse-integer "ff" :radix 16)
;;; => 255

;;; TEST: parse-integer with start/end
(parse-integer "hello123world" :start 5 :end 8)
;;; => 123

;;; TEST: parse-integer multiple values
(multiple-value-list (parse-integer "123 456"))
;;; => (123 3)

;;; TEST: format ~(~) downcase
(format nil "~(~A~)" "HELLO WORLD")
;;; => "hello world"

;;; TEST: format ~:P plural singular
(format nil "~D dog~:P" 1)
;;; => "1 dog"

;;; TEST: format ~:P plural multiple
(format nil "~D dog~:P" 3)
;;; => "3 dogs"

;;; TEST: format ~[~] conditional
(format nil "~[zero~;one~;two~]" 1)
;;; => "one"

;;; TEST: digit-char-p returns weight
(digit-char-p #\3)
;;; => 3

;;; TEST: digit-char-p hex
(digit-char-p #\a 16)
;;; => 10

;;; TEST: digit-char-p non-digit
(digit-char-p #\z)
;;; => nil

;;; TEST: with-input-from-string read-line
(with-input-from-string (s "hello world") (read-line s))
;;; => "hello world"

;;; TEST: format ~(~A~) downcase
(format nil "~(~A~)" "HELLO")
;;; => "hello"

;;; TEST: format ~:P singular
(format nil "~D dog~:P" 1)
;;; => "1 dog"

;;; TEST: format ~:P plural
(format nil "~D dog~:P" 3)
;;; => "3 dogs"

;;; TEST: loop AND parallel assignment fibonacci
(loop for i from 0 below 10 for a = 0 then b and b = 1 then (+ a b) finally (return a))
;;; => 55

;;; TEST: loop AND collect parallel values
(loop for i from 0 to 3 for a = 0 then b and b = 1 then (+ a b) collect (list a b))
;;; => ((0 1) (1 1) (1 2) (2 3))

;;; TEST: string-capitalize
(string-capitalize "hello world")
;;; => "Hello World"

;;; TEST: do* sequential stepping
(do* ((i 0 (1+ i)) (sum i (+ sum i))) ((= i 5) sum))
;;; => 15

;;; TEST: loop count
(loop for i from 1 to 10 count (evenp i))
;;; => 5

;;; TEST: loop maximize
(loop for i in '(3 1 4 1 5 9 2 6) maximize i)
;;; => 9

;;; TEST: loop minimize
(loop for i in '(3 1 4 1 5 9 2 6) minimize i)
;;; => 1

;;; TEST: loop thereis
(loop for i in '(1 3 5 6 7) thereis (evenp i))
;;; => t

;;; TEST: loop never
(loop for i in '(1 3 5 7) never (evenp i))
;;; => t

;;; TEST: loop always
(loop for i in '(2 4 6 8) always (evenp i))
;;; => t

;;; TEST: loop append
(loop for i in '(1 2 3) append (list i (* i i)))
;;; => (1 1 2 4 3 9)

;;; TEST: destructuring-bind
(destructuring-bind (a b &rest c) '(1 2 3 4 5) (list a b c))
;;; => (1 2 (3 4 5))

;;; TEST: ecase
(ecase 'b (a 1) (b 2) (c 3))
;;; => 2

;;; TEST: etypecase
(etypecase 42 (integer :int) (string :str))
;;; => :INT

;;; TEST: nsubst
(nsubst 'z 'a (list 'a 'b (list 'a 'c)))
;;; => (Z B (Z C))

;;; TEST: tree-equal
(tree-equal '(1 (2 3)) '(1 (2 3)))
;;; => t

;;; TEST: remove-duplicates
(length (remove-duplicates '(1 2 3 2 1 4)))
;;; => 4

;;; TEST: mismatch
(mismatch "abcdef" "abcxyz")
;;; => 3

;;; TEST: format ~{~^~} separator
(format nil "~{~A~^, ~}" '(1 2 3))
;;; => "1, 2, 3"

;;; TEST: mapcar multiple lists
(mapcar #'list '(1 2 3) '(a b c))
;;; => ((1 A) (2 B) (3 C))

;;; TEST: type-of various
(list (type-of 42) (type-of 3.14) (type-of "hi") (type-of 'x) (type-of #\a))
;;; => (FIXNUM FLOAT STRING SYMBOL CHARACTER)

;;; TEST: loop collect into named
(loop for i from 1 to 5 when (oddp i) collect i into odds when (evenp i) collect i into evens finally (return (list odds evens)))
;;; => ((1 3 5) (2 4))

;;; TEST: string-capitalize multi-word
(string-capitalize "hello WORLD foo")
;;; => "Hello World Foo"

;;; TEST: push in lambda captures
(let ((r nil)) (funcall (lambda () (push 1 r) (push 2 r))) r)
;;; => (2 1)

;;; TEST: incf in lambda captures
(let ((x 0)) (funcall (lambda () (incf x) (incf x))) x)
;;; => 2

;;; TEST: setf in lambda captures
(let ((x 0)) (funcall (lambda () (setf x 42))) x)
;;; => 42

;;; TEST: maphash with push
(let ((h (make-hash-table)) (r nil)) (setf (gethash 'a h) 1) (setf (gethash 'b h) 2) (maphash (lambda (k v) (push (cons k v) r)) h) (length r))
;;; => 2

;;; TEST: dolist push closure
(let ((result nil)) (dolist (x '(1 2 3)) (push (* x x) result)) (nreverse result))
;;; => (1 4 9)

;;; TEST: dotimes push closure
(let ((result nil)) (dotimes (i 5) (push i result)) (nreverse result))
;;; => (0 1 2 3 4)

;;; TEST: mapcar with incf state
(let ((n 0)) (mapcar (lambda (x) (incf n) (+ x n)) '(10 20 30)))
;;; => (11 22 33)

;;; TEST: counter closure
(let ((c (let ((n 0)) (lambda () (incf n) n)))) (list (funcall c) (funcall c) (funcall c)))
;;; => (1 2 3)

;;; TEST: format ~:[~]
(format nil "Active: ~:[no~;yes~]" t)
;;; => "Active: yes"

;;; TEST: flatten recursive
(labels ((flatten (lst) (cond ((null lst) nil) ((atom lst) (list lst)) (t (append (flatten (car lst)) (flatten (cdr lst))))))) (flatten '(1 (2 (3 4) 5) (6 7))))
;;; => (1 2 3 4 5 6 7)

;;; TEST: string join
(reduce (lambda (a b) (concatenate 'string a ", " b)) (mapcar #'princ-to-string '(1 2 3)))
;;; => "1, 2, 3"

;;; TEST: loop for on
(loop for x on '(1 2 3) collect (car x))
;;; => (1 2 3)

;;; TEST: accumulate with dolist incf
(let ((sum 0)) (dolist (x '(1 2 3 4 5)) (incf sum x)) sum)
;;; => 15

;;; TEST: format ~{ with pairs
(format nil "~{~A=~A ~}" '(a 1 b 2))
;;; => "A=1 B=2 "

;;; TEST: string= with escaped quotes
(string= "\"hi\"" "\"hi\"")
;;; => t

;;; TEST: format with escaped quotes
(format nil "\"~A\"" "hello")
;;; => ""hello""

;;; TEST: fill vector
(let ((v (make-array 3 :initial-element 0))) (fill v 42) (aref v 1))
;;; => 42

;;; TEST: rotatef
(let ((a 1) (b 2) (c 3)) (rotatef a b c) (list a b c))
;;; => (2 3 1)

;;; TEST: shiftf
(let ((a 1) (b 2) (c 3)) (shiftf a b c 4) (list a b c))
;;; => (2 3 4)

;;; TEST: complement
(remove-if (complement #'evenp) '(1 2 3 4 5))
;;; => (2 4)

;;; TEST: constantly
(mapcar (constantly 42) '(a b c))
;;; => (42 42 42)

;;; TEST: catch/throw
(catch 'done (throw 'done 42))
;;; => 42

;;; TEST: handler-bind with restart
(handler-bind ((error (lambda (c) (invoke-restart 'use-value 99)))) (restart-case (error "test") (use-value (v) v)))
;;; => 99

;;; TEST: loop across string
(loop for c across "abc" collect c)
;;; => (#\a #\b #\c)

;;; TEST: with-slots
(progn (defclass pt7 () ((x :initarg :x) (y :initarg :y))) (let ((p (make-instance 'pt7 :x 3 :y 4))) (with-slots (x y) p (+ x y))))
;;; => 7

;;; TEST: user function multiple values
(multiple-value-list (let ((mn 1) (mx 5)) (values mn mx)))
;;; => (1 5)

;;; TEST: nconc destructive
(let ((a (list 1 2)) (b (list 3 4))) (nconc a b) a)
;;; => (1 2 3 4)

;;; TEST: nconc return value
(nconc (list 1 2) (list 3 4))
;;; => (1 2 3 4)

;;; TEST: nconc nil first
(nconc nil (list 1 2))
;;; => (1 2)

;;; TEST: loop = then general
(loop for x = 1 then (* x 2) for i from 0 to 4 collect x)
;;; => (1 2 4 8 16)

;;; TEST: loop named return-from
(block outer (loop named inner for i from 1 to 10 when (> i 5) do (return-from inner i)))
;;; => 6

;;; TEST: loop initially/finally
(let ((result nil)) (loop initially (push :start result) for i from 1 to 3 do (push i result) finally (push :end result)) (nreverse result))
;;; => (:START 1 2 3 :END)

;;; TEST: defparameter
(progn (defparameter *tp1* 42) *tp1*)
;;; => 42

;;; TEST: assoc :test string=
(assoc "hello" '(("hello" . 1) ("world" . 2)) :test #'string=)
;;; => ("hello" . 1)

;;; TEST: sort :key
(mapcar #'car (sort (list '(3 "c") '(1 "a") '(2 "b")) #'< :key #'car))
;;; => (1 2 3)

;;; TEST: multiple-value-call
(multiple-value-call #'list (values 1 2) (values 3 4))
;;; => (1 2 3 4)

;;; TEST: unwind-protect cleanup
(let ((x nil)) (handler-case (unwind-protect (error "boom") (push :cleanup x)) (error (e) nil)) x)
;;; => (:CLEANUP)

;;; TEST: format ~{~A=~A~} pairs
(format nil "~{~A=~A ~}" '(a 1 b 2))
;;; => "A=1 B=2 "

;;; TEST: format ~:^ in iteration
(format nil "~{~A~:^, ~}" '(1 2 3))
;;; => "1, 2, 3"

;;; TEST: format ~:{~} sublists
(format nil "~:{(~A ~A)~}" '((a 1) (b 2)))
;;; => "(A 1)(B 2)"

;;; TEST: format ~:R ordinal
(format nil "~:R" 1)
;;; => "1st"

;;; TEST: format ~* skip arg
(format nil "~A ~* ~A" 1 2 3)
;;; => "1  3"

;;; TEST: subseq list
(subseq '(1 2 3 4 5) 1 3)
;;; => (2 3)

;;; TEST: subseq string
(subseq "hello world" 6)
;;; => "world"

;;; TEST: find-symbol
(symbolp (find-symbol "CAR"))
;;; => t

;;; TEST: map-into with lists
(let ((r (make-list 3))) (map-into r #'+ '(1 2 3) '(10 20 30)) r)
;;; => (11 22 33)

;;; TEST: format ~:{~} sublist iteration
(format nil "~:{~A:~A ~}" '((a 1) (b 2)))
;;; => "A:1 B:2 "

;;; TEST: format ~:^ in ~{~}
(format nil "~{~A~:^-~}" '(1 2 3))
;;; => "1-2-3"

;;; TEST: make-array :initial-contents
(let ((a (make-array 3 :initial-contents '(10 20 30)))) (list (aref a 0) (aref a 1) (aref a 2)))
;;; => (10 20 30)

;;; TEST: adjust-array
(let ((a (adjust-array (make-array 3 :initial-element 0) 5 :initial-element 99))) (list (aref a 0) (aref a 3)))
;;; => (0 99)

;;; TEST: class inheritance
(progn (defclass shape3 () ((color :initarg :color :accessor shape3-color))) (defclass circ3 (shape3) ((r :initarg :r :accessor circ3-r))) (let ((c (make-instance 'circ3 :color "red" :r 5))) (list (shape3-color c) (circ3-r c))))
;;; => ("red" 5)

;;; TEST: slot-value setf
(progn (defclass pt8 () ((x :initarg :x))) (let ((p (make-instance 'pt8 :x 1))) (setf (slot-value p 'x) 100) (slot-value p 'x)))
;;; => 100

;;; TEST: class-of
(progn (defclass pt9 () ()) (class-name (class-of (make-instance 'pt9))))
;;; => PT9

;;; TEST: typep user class
(progn (defclass pt10 () ()) (typep (make-instance 'pt10) 'pt10))
;;; => t

;;; TEST: string-upcase with :start/:end
(string-upcase "hello" :start 1 :end 3)
;;; => "hELlo"

;;; TEST: string-downcase with :start/:end
(string-downcase "HELLO" :start 1 :end 3)
;;; => "HelLO"

;;; TEST: format ~? indirect
(format nil "~?" "~D+~D" '(1 2))
;;; => "1+2"

;;; TEST: frequency counter
(let ((h (make-hash-table :test 'equal))) (dolist (w '("a" "b" "a" "a")) (setf (gethash w h) (1+ (or (gethash w h) 0)))) (gethash "a" h))
;;; => 3

;;; TEST: fib-cps
(labels ((fib-cps (n k) (if (<= n 1) (funcall k n) (fib-cps (- n 1) (lambda (v1) (fib-cps (- n 2) (lambda (v2) (funcall k (+ v1 v2))))))))) (fib-cps 10 #'identity))
;;; => 55

;;; TEST: reduce :initial-value
(reduce #'+ '(1 2 3 4 5) :initial-value 100)
;;; => 115

;;; TEST: mapcar multiple lists
(mapcar #'+ '(1 2 3) '(10 20 30))
;;; => (11 22 33)

;;; TEST: remove-if-not
(remove-if-not #'evenp '(1 2 3 4 5 6))
;;; => (2 4 6)

;;; TEST: copy-seq
(let ((a '(1 2 3)) (b (copy-seq '(1 2 3)))) (setf (car b) 99) (list a b))
;;; => ((1 2 3) (99 2 3))

;;; TEST: string-upcase whole string
(string-upcase "hello")
;;; => "HELLO"

;;; TEST: string-downcase whole string
(string-downcase "HELLO")
;;; => "hello"

;;; TEST: defstruct
(progn (defstruct person20 name age) (let ((p (make-person20 :name "Alice" :age 30))) (list (person20-name p) (person20-age p))))
;;; => ("Alice" 30)

;;; TEST: safe-divide
(handler-case (/ 10 0) (division-by-zero () :caught))
;;; => :CAUGHT

;;; TEST: class inheritance accessors
(progn (defclass sh4 () ((c :initarg :c :accessor sh4-c))) (defclass ci4 (sh4) ((r :initarg :r :accessor ci4-r))) (let ((x (make-instance 'ci4 :c "red" :r 5))) (list (sh4-c x) (ci4-r x))))
;;; => ("red" 5)

;;; TEST: defconstant
(progn (defconstant +c1+ 42) +c1+)
;;; => 42

;;; TEST: boundp
(list (progn (defvar *bp1* 1) (boundp '*bp1*)) (boundp '*nonexistent-xyz*))
;;; => (t nil)

;;; TEST: equalp case insensitive
(equalp "Hello" "hello")
;;; => t

;;; TEST: tree-equal
(tree-equal '(1 (2 3)) '(1 (2 3)))
;;; => t

;;; TEST: subst
(subst 99 2 '(1 2 (3 2 4)))
;;; => (1 99 (3 99 4))

;;; TEST: intersection
(sort (intersection '(1 2 3 4) '(2 4 6)) #'<)
;;; => (2 4)

;;; TEST: union
(sort (union '(1 2 3) '(2 3 4 5)) #'<)
;;; => (1 2 3 4 5)

;;; TEST: set-difference
(sort (set-difference '(1 2 3 4 5) '(2 4)) #'<)
;;; => (1 3 5)

;;; TEST: psetq
(let ((a 1) (b 2)) (psetq a b b a) (list a b))
;;; => (2 1)

;;; TEST: do parallel bindings
(do ((i 0 (1+ i)) (result nil)) ((= i 5) (nreverse result)) (push i result))
;;; => (0 1 2 3 4)

;;; TEST: case with multiple keys
(case 'monday ((monday tuesday) :weekday) ((saturday sunday) :weekend) (t :unknown))
;;; => :WEEKDAY

;;; TEST: typecase
(typecase 42 (integer :int) (string :str) (t :other))
;;; => :INT

;;; TEST: dynamic binding
(progn (defvar *dyn1* 10) (defun get-dyn1 () *dyn1*) (let ((*dyn1* 42)) (get-dyn1)))
;;; => 42

;;; TEST: format ~@{~} remaining args
(format nil "~@{~A~^, ~}" 1 2 3)
;;; => "1, 2, 3"

;;; TEST: format to string stream
(with-output-to-string (s) (format s "~A ~A" "Hello" "World"))
;;; => "Hello World"

;;; TEST: with-output-to-string format
(with-output-to-string (s) (format s "~D+~D=~D" 1 2 3))
;;; => "1+2=3"

;;; TEST: string-capitalize
(string-capitalize "hello world")
;;; => "Hello World"

;;; TEST: char-name
(char-name #\Space)
;;; => "Space"

;;; TEST: name-char
(name-char "Space")
;;; => #\space

;;; TEST: prog1
(prog1 1 2 3)
;;; => 1

;;; TEST: prog2
(prog2 1 2 3)
;;; => 2

;;; TEST: random bounds
(let ((r (random 10))) (and (>= r 0) (< r 10)))
;;; => t

;;; TEST: array-dimensions
(array-dimensions (make-array 5))
;;; => (5)

;;; TEST: array-total-size
(array-total-size (make-array 5))
;;; => 5

;;; TEST: nsubstitute destructive
(let ((l (list 1 2 3 2 1))) (nsubstitute 99 2 l) l)
;;; => (1 99 3 99 1)

;;; TEST: read from string stream
(with-input-from-string (s "(+ 1 2)") (eval (read s)))
;;; => 3

;;; TEST: read from string stream symbol
(with-input-from-string (s "hello") (read s))
;;; => HELLO

;;; TEST: with-output-to-string format
(with-output-to-string (s) (format s "~D items" 42))
;;; => "42 items"

;;; TEST: split-string helper
(labels ((split (str sep) (let ((r nil) (start 0)) (dotimes (i (length str)) (when (char= (char str i) sep) (push (subseq str start i) r) (setq start (1+ i)))) (push (subseq str start (length str)) r) (nreverse r)))) (split "a,b,c" #\,))
;;; => ("a" "b" "c")

;;; TEST: accumulator closure
(let ((acc (let ((total 0)) (lambda (n) (incf total n) total)))) (list (funcall acc 10) (funcall acc 20) (funcall acc 30)))
;;; => (10 30 60)

;;; TEST: fizzbuzz
(loop for i from 1 to 5 collect (cond ((zerop (mod i 15)) "FB") ((zerop (mod i 3)) "F") ((zerop (mod i 5)) "B") (t i)))
;;; => (1 2 "F" 4 "B")

;;; TEST: loop maximize
(loop for i in '(3 1 4 1 5 9) maximize i)
;;; => 9

;;; TEST: loop minimize
(loop for i in '(3 1 4 1 5 9) minimize i)
;;; => 1

;;; TEST: getf
(getf '(:a 1 :b 2 :c 3) :b)
;;; => 2

;;; TEST: count-if
(count-if #'evenp '(1 2 3 4 5 6))
;;; => 3

;;; TEST: format ~W (write)
(format nil "~W" '(1 2 3))
;;; => "(1 2 3)"

;;; TEST: format ~VA (variable width)
(format nil "~VA" 10 "hi")
;;; => "hi        "

;;; TEST: nsubstitute-if destructive
(let ((l (list 1 2 3 4 5))) (nsubstitute-if 0 #'evenp l) l)
;;; => (1 0 3 0 5)

;;; TEST: with-hash-table-iterator
(let ((h (make-hash-table))) (setf (gethash 'a h) 1) (with-hash-table-iterator (next h) (multiple-value-bind (more key val) (next) (list more val))))
;;; => (t 1)

;;; TEST: delete
(delete 2 (list 1 2 3 2 1))
;;; => (1 3 1)

;;; TEST: delete-if
(delete-if #'evenp (list 1 2 3 4 5))
;;; => (1 3 5)

;;; TEST: pushnew
(let ((l '(1 2 3))) (pushnew 2 l) (pushnew 4 l) l)
;;; => (4 1 2 3)

;;; TEST: acons
(acons 'a 1 '((b . 2)))
;;; => ((A . 1) (B . 2))

;;; TEST: stable-sort
(stable-sort (list 3 1 4 1 5) #'<)
;;; => (1 1 3 4 5)

;;; TEST: merge lists
(merge 'list '(1 3 5) '(2 4 6) #'<)
;;; => (1 2 3 4 5 6)

;;; TEST: string-equal case-insensitive
(string-equal "Hello" "hello")
;;; => t

;;; TEST: coerce string to list
(coerce "abc" 'list)
;;; => (#\a #\b #\c)

;;; TEST: equalp hash-table
(let ((h (make-hash-table :test 'equalp))) (setf (gethash "Hello" h) 1) (gethash "hello" h))
;;; => 1

;;; TEST: gethash multiple values found
(let ((h (make-hash-table))) (setf (gethash 'x h) 42) (multiple-value-list (gethash 'x h)))
;;; => (42 t)

;;; TEST: gethash multiple values not found
(let ((h (make-hash-table))) (multiple-value-list (gethash 'y h)))
;;; => (nil nil)

;;; TEST: intern secondary value
(multiple-value-list (intern "CAR"))
;;; => (CAR :INTERNAL)

;;; TEST: loop thereis
(loop for i in '(1 3 5 4 7) thereis (evenp i))
;;; => t

;;; TEST: loop never
(loop for i in '(1 3 5 7) never (evenp i))
;;; => t

;;; TEST: loop always
(loop for i in '(1 3 5 7) always (oddp i))
;;; => t

;;; TEST: map nil side effects
(let ((result nil)) (map nil (lambda (x) (push x result)) '(1 2 3)) (nreverse result))
;;; => (1 2 3)

;;; TEST: locally
(locally (+ 1 2))
;;; => 3

;;; TEST: reduce
(reduce #'+ '(1 2 3 4 5))
;;; => 15

;;; TEST: reduce initial-value
(reduce #'+ '(1 2 3) :initial-value 10)
;;; => 16

;;; TEST: position
(position 3 '(1 2 3 4 5))
;;; => 2

;;; TEST: position-if
(position-if #'evenp '(1 3 5 4 7))
;;; => 3

;;; TEST: ash
(list (ash 1 10) (ash 1024 -3))
;;; => (1024 128)

;;; TEST: ldb byte
(ldb (byte 4 0) 255)
;;; => 15

;;; TEST: concatenate vectors
(concatenate 'vector #(1 2) #(3 4))
;;; => #(1 2 3 4)

;;; TEST: coerce list to vector
(coerce '(1 2 3) 'vector)
;;; => #(1 2 3)

;;; TEST: do macro
(do ((i 0 (1+ i)) (sum 0 (+ sum i))) ((= i 5) sum))
;;; => 10

;;; TEST: do*
(do* ((i 0 (1+ i)) (sum 0 (+ sum i))) ((= i 5) sum))
;;; => 15

;;; TEST: dotimes return value
(dotimes (i 5 42) nil)
;;; => 42

;;; TEST: defstruct basic
(progn (defstruct point-99 x y) (let ((p (make-point-99 :x 3 :y 4))) (point-99-x p)))
;;; => 3

;;; TEST: typecase
(typecase 42 (string "str") (integer "int") (t "other"))
;;; => "int"

;;; TEST: ecase
(ecase 'b (a 1) (b 2) (c 3))
;;; => 2

;;; TEST: destructuring-bind
(destructuring-bind (a (b c) &rest d) '(1 (2 3) 4 5) (list a b c d))
;;; => (1 2 3 (4 5))

;;; TEST: values-list
(multiple-value-list (values-list '(1 2 3)))
;;; => (1 2 3)

;;; TEST: nth-value
(nth-value 1 (floor 7 2))
;;; => 1

;;; TEST: complement
(funcall (complement #'evenp) 3)
;;; => t

;;; TEST: constantly
(funcall (constantly 42) 1 2 3)
;;; => 42

;;; TEST: unwind-protect runs cleanup
(let ((cleaned nil)) (handler-case (unwind-protect (error "oops") (setq cleaned t)) (error (e) nil)) cleaned)
;;; => t

;;; TEST: with-open-file roundtrip
(progn (with-open-file (s "/tmp/habu-snap.txt" :direction :output :if-exists :supersede) (format s "snap")) (with-open-file (s "/tmp/habu-snap.txt" :direction :input) (read-line s)))
;;; => "snap"

;;; TEST: initialize-instance :after
(progn (defclass init-test-99 () ((val :initarg :val :initform 0 :accessor init-test-99-val))) (defmethod initialize-instance :after ((obj init-test-99) &rest args) (declare (ignore args)) (setf (init-test-99-val obj) 42)) (init-test-99-val (make-instance 'init-test-99)))
;;; => 42

;;; TEST: defmethod :before
(progn (defclass bef-test () ((val :initarg :val :accessor bef-test-val))) (defgeneric proc-bef (o)) (defmethod proc-bef ((o bef-test)) (bef-test-val o)) (defvar *bef-flag* nil) (defmethod proc-bef :before ((o bef-test)) (setq *bef-flag* t)) (let ((r (proc-bef (make-instance 'bef-test :val 10)))) (list r *bef-flag*)))
;;; => (10 t)

;;; TEST: defmethod :around
(progn (defclass ar-test () ()) (defgeneric proc-ar (o)) (defmethod proc-ar ((o ar-test)) 42) (defmethod proc-ar :around ((o ar-test)) (+ (call-next-method) 1)) (proc-ar (make-instance 'ar-test)))
;;; => 43

;;; TEST: remhash
(let ((h (make-hash-table))) (setf (gethash 'k h) 1) (remhash 'k h) (gethash 'k h))
;;; => nil

;;; TEST: hash-table-count
(let ((h (make-hash-table))) (setf (gethash 'a h) 1) (setf (gethash 'b h) 2) (hash-table-count h))
;;; => 2

;;; TEST: clrhash
(let ((h (make-hash-table))) (setf (gethash 'a h) 1) (clrhash h) (hash-table-count h))
;;; => 0

;;; TEST: last
(last '(1 2 3))
;;; => (3)

;;; TEST: butlast
(butlast '(1 2 3 4 5))
;;; => (1 2 3 4)

;;; TEST: nbutlast
(nbutlast (list 1 2 3 4 5))
;;; => (1 2 3 4)

;;; TEST: ldiff
(let ((l '(1 2 3 4 5))) (ldiff l (cddr l)))
;;; => (1 2)

;;; TEST: tailp
(let ((l '(1 2 3))) (tailp (cdr l) l))
;;; => t

;;; TEST: make-list
(make-list 3 :initial-element 'x)
;;; => (X X X)

;;; TEST: copy-tree
(let ((tree '((1 2) (3 4)))) (let ((copy (copy-tree tree))) (eq (car tree) (car copy))))
;;; => nil

;;; TEST: tree-equal
(tree-equal '(1 (2 3)) '(1 (2 3)))
;;; => t

;;; TEST: subst
(subst 'x 'a '(a b (a c)))
;;; => (X B (X C))

;;; TEST: copy-list
(let ((a '(1 2 3))) (let ((b (copy-list a))) (eq a b)))
;;; => nil

;;; TEST: map vector result
(map 'vector #'+ #(1 2 3) #(10 20 30))
;;; => #(11 22 33)

;;; TEST: pairlis
(pairlis '(a b c) '(1 2 3))
;;; => ((A . 1) (B . 2) (C . 3))

;;; TEST: format ~T tabulate
(length (format nil "a~10Tb"))
;;; => 11

;;; TEST: loop across string
(loop for c across "abc" collect c)
;;; => (#\a #\b #\c)

;;; TEST: loop across vector
(loop for x across #(1 2 3) sum x)
;;; => 6

;;; TEST: loop destructuring
(loop for (a b) in '((1 2) (3 4)) collect (+ a b))
;;; => (3 7)

;;; TEST: slot-value
(progn (defclass sv-snap () ((x :initarg :x))) (slot-value (make-instance 'sv-snap :x 42) 'x))
;;; => 42

;;; TEST: setf slot-value
(progn (defclass svs-snap () ((x :initarg :x :accessor svs-snap-x))) (let ((o (make-instance 'svs-snap :x 0))) (setf (slot-value o 'x) 99) (slot-value o 'x)))
;;; => 99

;;; TEST: type-of
(list (type-of 42) (type-of "hi") (type-of 'x))
;;; => (FIXNUM STRING SYMBOL)

;;; TEST: string function
(list (string 'hello) (string "test"))
;;; => ("HELLO" "test")

;;; TEST: multiple-value-call
(multiple-value-call #'list (floor 7 2) (floor 9 4))
;;; => (3 1 2 1)

;;; TEST: apply multiple args
(apply #'+ 1 2 '(3 4))
;;; => 10

;;; TEST: with-gensyms macro
(progn (defmacro wg-test (syms &rest body) `(let ,(mapcar (lambda (s) `(,s (gensym))) syms) ,@body)) (wg-test (a b) (list (symbolp a) (symbolp b))))
;;; => (t t)

;;; TEST: remove-duplicates :key
(remove-duplicates '((a . 1) (b . 2) (a . 3)) :key #'car)
;;; => ((B . 2) (A . 3))

;;; TEST: mapc single list
(let ((r nil)) (mapc (lambda (x) (push x r)) '(1 2 3)) (nreverse r))
;;; => (1 2 3)

;;; TEST: mapc return value
(mapc (lambda (x) nil) '(1 2 3))
;;; => (1 2 3)

;;; TEST: mapcan
(mapcan (lambda (x) (if (evenp x) (list x) nil)) '(1 2 3 4 5 6))
;;; => (2 4 6)

;;; TEST: intersection
(sort (intersection '(1 2 3 4) '(3 4 5 6)) #'<)
;;; => (3 4)

;;; TEST: union
(length (union '(1 2 3) '(3 4 5)))
;;; => 5

;;; TEST: set-difference
(sort (set-difference '(1 2 3 4) '(3 4 5)) #'<)
;;; => (1 2)

;;; TEST: subsetp
(subsetp '(1 2) '(1 2 3 4))
;;; => t

;;; TEST: coerce char to string
(coerce #\a 'string)
;;; => "a"

;;; TEST: char-code
(char-code #\A)
;;; => 65

;;; TEST: code-char
(code-char 65)
;;; => #\A

;;; TEST: write-to-string
(write-to-string 42)
;;; => "42"

;;; TEST: prin1-to-string
(prin1-to-string '(1 2 3))
;;; => "(1 2 3)"

;;; TEST: loop hash-keys
(let ((h (make-hash-table))) (setf (gethash 'a h) 1) (setf (gethash 'b h) 2) (sort (loop for k being the hash-keys of h collect k) #'string<))
;;; => (A B)

;;; TEST: loop across string collect char-code
(loop for c across "Hi" collect (char-code c))
;;; => (72 105)

;;; TEST: format ~& fresh-line
(length (format nil "a~&b"))
;;; => 3

;;; TEST: format ~% newline
(length (format nil "a~%b"))
;;; => 3

;;; TEST: flatten
(labels ((flat (tr) (cond ((null tr) nil) ((atom tr) (list tr)) (t (append (flat (car tr)) (flat (cdr tr))))))) (flat '(1 (2 (3 4) 5) (6 7))))
;;; => (1 2 3 4 5 6 7)

;;; TEST: compose
(progn (defun compose2 (f g) (lambda (&rest args) (funcall f (apply g args)))) (funcall (compose2 #'1+ #'abs) -5))
;;; => 6

;;; TEST: return-from nested block
(block outer (dolist (x '(1 2 3 4 5)) (when (= x 3) (return-from outer (* x 10)))))
;;; => 30

;;; TEST: nested hash tables
(let ((db (make-hash-table :test 'equal))) (setf (gethash "u" db) (make-hash-table :test 'equal)) (setf (gethash "a" (gethash "u" db)) 42) (gethash "a" (gethash "u" db)))
;;; => 42

;;; TEST: count
(count 2 '(1 2 3 2 1))
;;; => 2

;;; TEST: find
(find 3 '(1 2 3 4 5))
;;; => 3

;;; TEST: find-if
(find-if #'evenp '(1 3 5 4 7))
;;; => 4

;;; TEST: remove-if-not
(remove-if-not #'oddp '(1 2 3 4 5))
;;; => (1 3 5)

;;; TEST: char comparisons
(list (char< #\a #\b) (char> #\b #\a) (char= #\a #\a))
;;; => (t t t)

;;; TEST: position-if-not
(position-if-not #'alpha-char-p "hello123")
;;; => 5

;;; TEST: string-left-trim character bag
(string-left-trim "abc" "abcxyz")
;;; => "xyz"

;;; TEST: loop across with when
(loop for i from 0 below 6 for c across "ABCDEF" when (evenp i) collect c)
;;; => (#\A #\C #\E)

;;; TEST: handler-bind
(handler-case (handler-bind ((error (lambda (c) (invoke-restart 'continue)))) 42) (error (e) :err))
;;; => 42
