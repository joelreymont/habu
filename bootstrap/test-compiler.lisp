;; Generate tiny placeholder binaries for test_compiler_simple.c
(let* ((tmpdir (or (sb-ext:posix-getenv "TMPDIR") "/tmp"))
       (x86-path (merge-pathnames "test-x86_64.bin" (parse-namestring tmpdir)))
       (arm-path (merge-pathnames "test-arm64.bin" (parse-namestring tmpdir)))
       (x86-bytes (list #xC3))          ; ret
       (arm-bytes (list #xC0 #x03 #x5F #xD6))) ; ret
  (flet ((write-bytes (path bytes)
           (with-open-file (out path :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create
                                     :element-type '(unsigned-byte 8))
             (dolist (b bytes)
               (write-byte b out)))))
    (write-bytes x86-path x86-bytes)
    (write-bytes arm-path arm-bytes)))
