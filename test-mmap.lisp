#!/usr/bin/env sbcl --script

(require :sb-posix)

(format t "Testing mmap with PROT_EXEC...~%")

(let* ((prot (logior sb-posix:prot-read
                     sb-posix:prot-write
                     sb-posix:prot-exec))
       (flags (logior sb-posix:map-private
                      sb-posix:map-anon)))

  (format t "PROT flags: ~A~%" prot)
  (format t "MAP flags: ~A~%" flags)

  (handler-case
      (let ((sap (sb-posix:mmap nil 4096 prot flags -1 0)))
        (format t "Success! Allocated executable memory at: ~A~%" (sb-sys:sap-int sap))
        (sb-posix:munmap sap 4096)
        (format t "Success! Freed memory.~%"))
    (error (e)
      (format t "Failed: ~A~%" e))))
