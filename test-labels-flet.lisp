;; Test LABELS and FLET implementation in h0-compile

;; Test 1: Simple FLET with non-recursive function
(flet ((add-five (x) (+ x 5)))
  (add-five 10))
;; Expected: 15

;; Test 2: Simple LABELS with recursive function (factorial)
(labels ((fact (n)
           (if (= n 0)
               1
               (* n (fact (- n 1))))))
  (fact 5))
;; Expected: 120

;; Test 3: LABELS with mutually recursive functions
(labels ((even-p (n)
           (if (= n 0)
               t
               (odd-p (- n 1))))
         (odd-p (n)
           (if (= n 0)
               nil
               (even-p (- n 1)))))
  (even-p 4))
;; Expected: t (1)

;; Test 4: FLET with multiple functions
(flet ((double (x) (* x 2))
       (triple (x) (* x 3)))
  (+ (double 5) (triple 4)))
;; Expected: 22

;; Test 5: LABELS with closure over outer variable
(let ((base 10))
  (labels ((add-base (x) (+ x base)))
    (add-base 5)))
;; Expected: 15
