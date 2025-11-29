;;; Test simple string-append with new implementation

(let ((result (string-append "A" "B")))
  (sys-write 1 result (string-length result))
  (sys-write 1 "\n" 1)
  (sys-exit 42))
