;;;; Runtime list operations for Habu
;;;;
;;;; Provides basic list operations: length, nth, append, reverse

(in-package :habu-runtime)

(export '(runtime-length
          runtime-nth
          runtime-append
          runtime-reverse
          runtime-last
          runtime-butlast
          runtime-nthcdr
          runtime-member
          runtime-assoc
          runtime-position
          runtime-count
          runtime-remove))

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

;;; Butlast - all but last N elements
(defun runtime-butlast (list-ptr &optional (n-tagged #x10))
  "Get all but the last N elements (N defaults to 1)"
  ;; Extract N from tagged fixnum
  (let* ((n (ash n-tagged -4))
         (len (runtime-length list-ptr)))
    (if (<= len n)
        0  ; Return nil if list too short
        ;; Copy first (len - n) elements
        (let ((target-len (- len n)))
          (labels ((copy-n (ptr count)
                     (if (or (zerop count) (zerop ptr))
                         0
                         (let* ((header-addr (logand ptr (lognot #xF)))
                                (car-val (read-u64 *heap* (+ header-addr 8)))
                                (cdr-val (read-u64 *heap* (+ header-addr 16))))
                           (runtime-cons car-val (copy-n cdr-val (1- count)))))))
            (copy-n list-ptr target-len))))))

;;; Nthcdr - skip N elements
(defun runtime-nthcdr (n-tagged list-ptr)
  "Skip N elements and return the rest"
  (let ((n (ash n-tagged -4))
        (current list-ptr))
    (loop repeat n
          while (and (not (zerop current))
                     (= (logand current #xF) +tag-cons+))
          do (setf current (read-u64 *heap* (+ (logand current (lognot #xF)) 16))))
    current))

;;; Member - test if element is in list
(defun runtime-member (item list-ptr)
  "Find first occurrence of item in list, return tail from that point"
  (let ((current list-ptr))
    (loop while (and (not (zerop current))
                     (= (logand current #xF) +tag-cons+))
          do (let* ((header-addr (logand current (lognot #xF)))
                    (car-val (read-u64 *heap* (+ header-addr 8)))
                    (cdr-val (read-u64 *heap* (+ header-addr 16))))
               (when (= car-val item)
                 (return-from runtime-member current))
               (setf current cdr-val)))
    0))  ; Not found

;;; Assoc - find key in association list
(defun runtime-assoc (key alist-ptr)
  "Find first pair with matching key in association list"
  (let ((current alist-ptr))
    (loop while (and (not (zerop current))
                     (= (logand current #xF) +tag-cons+))
          do (let* ((header-addr (logand current (lognot #xF)))
                    (car-val (read-u64 *heap* (+ header-addr 8)))
                    (cdr-val (read-u64 *heap* (+ header-addr 16))))
               ;; car-val should be a cons pair (key . value)
               (when (and (not (zerop car-val))
                         (= (logand car-val #xF) +tag-cons+))
                 (let* ((pair-addr (logand car-val (lognot #xF)))
                        (pair-key (read-u64 *heap* (+ pair-addr 8))))
                   (when (= pair-key key)
                     (return-from runtime-assoc car-val))))
               (setf current cdr-val)))
    0))  ; Not found

;;; Position - find index of element
(defun runtime-position (item list-ptr)
  "Find 0-based index of first occurrence, return as tagged fixnum or 0 (nil)"
  (let ((current list-ptr)
        (index 0))
    (loop while (and (not (zerop current))
                     (= (logand current #xF) +tag-cons+))
          do (let* ((header-addr (logand current (lognot #xF)))
                    (car-val (read-u64 *heap* (+ header-addr 8)))
                    (cdr-val (read-u64 *heap* (+ header-addr 16))))
               (when (= car-val item)
                 (return-from runtime-position (ash index 4)))  ; Return tagged fixnum
               (setf current cdr-val)
               (incf index)))
    0))  ; Not found

;;; Count - count occurrences of element
(defun runtime-count (item list-ptr)
  "Count occurrences of item in list, return as tagged fixnum"
  (let ((current list-ptr)
        (count 0))
    (loop while (and (not (zerop current))
                     (= (logand current #xF) +tag-cons+))
          do (let* ((header-addr (logand current (lognot #xF)))
                    (car-val (read-u64 *heap* (+ header-addr 8)))
                    (cdr-val (read-u64 *heap* (+ header-addr 16))))
               (when (= car-val item)
                 (incf count))
               (setf current cdr-val)))
    (ash count 4)))  ; Return as tagged fixnum

;;; Remove - remove all occurrences of element
(defun runtime-remove (item list-ptr)
  "Remove all occurrences of item from list, return new list"
  (if (zerop list-ptr)
      0  ; Empty list
      (let* ((header-addr (logand list-ptr (lognot #xF)))
             (car-val (read-u64 *heap* (+ header-addr 8)))
             (cdr-val (read-u64 *heap* (+ header-addr 16)))
             (rest-removed (runtime-remove item cdr-val)))
        (if (= car-val item)
            rest-removed  ; Skip this element
            (runtime-cons car-val rest-removed)))))
