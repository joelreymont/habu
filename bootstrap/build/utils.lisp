;;; Pure Habu Utilities - No SBCL dependencies
;;; List operations using only Habu primitives

(in-package :habu)

(defun pure-append (lst1 lst2)
  "Append two lists"
  (labels ((append-iter (l acc)
             (if (null l)
                 acc
                 (append-iter (cdr l) (cons (car l) acc)))))
    (append-iter (pure-reverse lst1) lst2)))

(defun pure-reverse (lst)
  "Reverse a list"
  (labels ((rev-iter (l acc)
             (if (null l)
                 acc
                 (rev-iter (cdr l) (cons (car l) acc)))))
    (rev-iter lst nil)))

(defun pure-length (lst)
  "List length"
  (labels ((len-iter (l n)
             (if (null l)
                 n
                 (len-iter (cdr l) (+ n 1)))))
    (len-iter lst 0)))

(defun pure-nth (n lst)
  "Get nth element"
  (if (= n 0)
      (car lst)
      (pure-nth (- n 1) (cdr lst))))

(defun pure-mapcar (fn lst)
  "Map function over list"
  (labels ((map-iter (l acc)
             (if (null l)
                 (pure-reverse acc)
                 (map-iter (cdr l) (cons (funcall fn (car l)) acc)))))
    (map-iter lst nil)))

(defun pure-member (item lst)
  "Check if item in list"
  (labels ((mem-iter (l)
             (if (null l)
                 nil
                 (if (eq (car l) item)
                     l
                     (mem-iter (cdr l))))))
    (mem-iter lst)))

(defun pure-assoc (key alist)
  "Lookup key in association list"
  (labels ((assoc-iter (l)
             (if (null l)
                 nil
                 (if (eq (car (car l)) key)
                     (car l)
                     (assoc-iter (cdr l))))))
    (assoc-iter alist)))

;;; Export utilities
(export '(pure-append pure-reverse pure-length pure-nth 
          pure-mapcar pure-member pure-assoc) :habu)
