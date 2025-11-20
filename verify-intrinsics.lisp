;;;; Verify Habu ARM64 Intrinsics Generate Correct Bytes
;;;; This tests that our Lisp intrinsics produce the exact same bytes as verified

(load "habu-arm64-codegen.lisp")

;;; Test helper - compare byte lists
(defun bytes-equal? (b1 b2)
  (if (cons? b1)
    (if (cons? b2)
      (if (= (car b1) (car b2))
        (bytes-equal? (cdr b1) (cdr b2))
        nil)
      nil)
    (if (cons? b2)
      nil
      (quote t))))  ; Both nil

;;; Print a byte list
(defun print-bytes (bytes)
  (if (cons? bytes)
    (progn
      (print (car bytes))
      (print " ")
      (print-bytes (cdr bytes)))
    nil))

;;; Test function
(defun test-intrinsic (name generated expected)
  (print name)
  (print ": ")
  (if (bytes-equal? generated expected)
    (print "✓ PASS")
    (progn
      (print "✗ FAIL - got ")
      (print-bytes generated)
      (print ", expected ")
      (print-bytes expected)))
  (newline))

;;; Run tests
(print "Verifying ARM64 Intrinsics")
(newline)
(newline)

;;; Test movz x0, #672 (tagged 42)
(test-intrinsic "movz x0, #672"
  (arm64-movz 0 672)
  (quote (0 84 128 210)))

;;; Test movz x0, #48 (tagged 3)
(test-intrinsic "movz x0, #48"
  (arm64-movz 0 48)
  (quote (0 6 128 210)))

;;; Test movz x0, #64 (tagged 4)
(test-intrinsic "movz x0, #64"
  (arm64-movz 0 64)
  (quote (0 8 128 210)))

;;; Test movz x1, #100
(test-intrinsic "movz x1, #100"
  (arm64-movz 1 100)
  (quote (129 12 128 210)))

;;; Test add x0, x0, x1
(test-intrinsic "add x0, x0, x1"
  (arm64-add 0 0 1)
  (quote (0 0 1 139)))

;;; Test sub x0, x0, x1
(test-intrinsic "sub x0, x0, x1"
  (arm64-sub 0 0 1)
  (quote (0 0 1 203)))

;;; Test mul x0, x0, x1
(test-intrinsic "mul x0, x0, x1"
  (arm64-mul 0 0 1)
  (quote (0 124 1 155)))

;;; Test lsr x0, x0, #4
(test-intrinsic "lsr x0, x0, #4"
  (arm64-lsr 0 0 4)
  (quote (0 252 68 211)))

;;; Test lsl x0, x0, #4
(test-intrinsic "lsl x0, x0, #4"
  (arm64-lsl 0 0 4)
  (quote (0 236 124 211)))

;;; Test mov x1, x0
(test-intrinsic "mov x1, x0"
  (arm64-mov 1 0)
  (quote (225 3 0 170)))

;;; Test ret
(test-intrinsic "ret"
  (arm64-ret)
  (quote (192 3 95 214)))

(print "Done")
(newline)
