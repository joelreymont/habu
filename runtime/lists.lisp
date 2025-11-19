;;;; Runtime list operations for Habu
;;;;
;;;; Provides basic list operations: length, nth, append, reverse

(in-package :habu-runtime)

(export '(runtime-length
          runtime-nth
          runtime-append
          runtime-reverse
          runtime-last))

;;; List length
(defun runtime-length (list-ptr)
  "Count the number of elements in a list"
  (let ((count 0)
        (current list-ptr))
    (loop while (and (not (zerop current))
                     (= (logand current #xF) +tag-cons+))
          do (incf count)
             (setf current (read-u64 *heap* (+ (logand current (lognot #xF)) 16))))
    count))

;;; List nth element (0-indexed)
(defun runtime-nth (n list-ptr)
  "Get the nth element of a list (0-indexed)"
  (let ((current list-ptr)
        (index 0))
    (loop while (and (< index n)
                     (not (zerop current))
                     (= (logand current #xF) +tag-cons+))
          do (incf index)
             (setf current (read-u64 *heap* (+ (logand current (lognot #xF)) 16))))
    (if (and (= index n)
             (not (zerop current))
             (= (logand current #xF) +tag-cons+))
        ;; Return the car of the current cons cell
        (read-u64 *heap* (+ (logand current (lognot #xF)) 8))
        (error "Index ~D out of range for list" n))))

;;; List append (concatenate two lists)
(defun runtime-append (list1-ptr list2-ptr)
  "Concatenate two lists"
  (if (zerop list1-ptr)
      ;; First list is empty, return second list
      list2-ptr
      ;; First list is not empty, cons car of list1 onto (append (cdr list1) list2)
      (let* ((header-addr (logand list1-ptr (lognot #xF)))
             (car-val (read-u64 *heap* (+ header-addr 8)))
             (cdr-val (read-u64 *heap* (+ header-addr 16)))
             (rest-appended (runtime-append cdr-val list2-ptr)))
        (runtime-cons car-val rest-appended))))

;;; List reverse
(defun runtime-reverse (list-ptr)
  "Reverse a list"
  (let ((result 0)  ; Start with nil
        (current list-ptr))
    (loop while (and (not (zerop current))
                     (= (logand current #xF) +tag-cons+))
          do (let* ((header-addr (logand current (lognot #xF)))
                    (car-val (read-u64 *heap* (+ header-addr 8)))
                    (cdr-val (read-u64 *heap* (+ header-addr 16))))
               ;; cons car onto result
               (setf result (runtime-cons car-val result))
               ;; move to next element
               (setf current cdr-val)))
    result))

;;; Last element of a list
(defun runtime-last (list-ptr)
  "Get the last cons cell of a list"
  (if (zerop list-ptr)
      0  ; nil
      (let ((current list-ptr))
        (loop while (let* ((header-addr (logand current (lognot #xF)))
                          (cdr-val (read-u64 *heap* (+ header-addr 16))))
                     (and (not (zerop cdr-val))
                          (= (logand cdr-val #xF) +tag-cons+)))
              do (setf current (read-u64 *heap* (+ (logand current (lognot #xF)) 16))))
        current)))
