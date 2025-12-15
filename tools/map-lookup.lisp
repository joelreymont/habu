;;;; map-lookup.lisp - Find function containing address in .map file
;;;; Usage: sbcl --script tools/map-lookup.lisp mapfile address

(defun parse-hex (str)
  "Parse hex string (with or without 0x prefix) to integer."
  (let ((s (if (and (> (length str) 2) (string= "0x" (subseq str 0 2)))
               (subseq str 2)
               str)))
    (parse-integer s :radix 16 :junk-allowed t)))

(defun split-whitespace (str)
  "Split string on whitespace, return list of non-empty parts."
  (let ((result nil) (start 0) (len (length str)))
    (loop for i from 0 to len
          do (when (or (= i len)
                       (member (char str i) '(#\Space #\Tab)))
               (when (> i start)
                 (push (subseq str start i) result))
               (setf start (1+ i))))
    (nreverse result)))

(defun find-function (map-file addr)
  "Find function containing address."
  (let ((addr-num (if (stringp addr) (parse-hex addr) addr))
        (prev-name nil))
    (with-open-file (s map-file)
      (loop for line = (read-line s nil nil)
            while line
            do (let ((parts (split-whitespace line)))
                 (when (and (first parts)
                            (> (length (first parts)) 2)
                            (string= "0x" (subseq (first parts) 0 2)))
                   (let ((n (parse-hex (first parts))))
                     (when (and n addr-num (> n addr-num))
                       (return prev-name))
                     (setf prev-name (second parts)))))))
    prev-name))

(defun main ()
  (let* ((args (cdr sb-ext:*posix-argv*))
         (map-file (first args))
         (addr (second args)))
    (unless (and map-file addr)
      (format t "Usage: map-lookup.lisp mapfile address~%")
      (sb-ext:exit :code 1))
    (let ((fn (find-function map-file addr)))
      (if fn
          (format t "~A~%" fn)
          (format t "Not found~%")))))

(main)
(sb-ext:exit :code 0)
