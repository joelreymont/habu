;; Test string intrinsics directly
(defun test-string-ops ()
  (let ((s "ABC"))
    (sys-write-string "String: ")
    (sys-write-string s)
    (sys-write-string "\n")

    (sys-write-string "Length: ")
    (sys-write-int (string-length s))
    (sys-write-string "\n")

    (sys-write-string "Char 0: ")
    (sys-write-int (string-ref s 0))
    (sys-write-string "\n")

    (sys-write-string "Char 1: ")
    (sys-write-int (string-ref s 1))
    (sys-write-string "\n")

    (sys-write-string "Char 2: ")
    (sys-write-int (string-ref s 2))
    (sys-write-string "\n")))

(test-string-ops)
