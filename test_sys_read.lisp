;;; Test basic sys-read functionality

(let* ((path "/tmp/small.txt")
       (fd (sys-open path #x0 0))  ;; O_RDONLY
       (buf (make-vector 100)))
  (let ((n (sys-read fd buf 100)))
    (sys-close fd)
    (sys-write 1 "Read " 5)
    (sys-write 1 (number-to-string n)
               (string-length (number-to-string n)))
    (sys-write 1 " bytes\n" 7)
    (sys-exit (if (= n 8) 42 1))))
