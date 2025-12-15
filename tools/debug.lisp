;;;; debug.lisp - Run binary under lldb and capture crash info
;;;; Usage: sbcl --script tools/debug.lisp binary [stdin-input]

(defun run-cmd (cmd)
  "Run shell command and return output as string."
  (with-output-to-string (out)
    (sb-ext:run-program "/bin/sh" (list "-c" cmd) :output out :error out)))

(defun main ()
  (let* ((args (cdr sb-ext:*posix-argv*))
         (binary (first args))
         (stdin-input (or (second args) "")))
    (unless binary
      (format t "Usage: debug.lisp binary [stdin-input]~%")
      (sb-ext:exit :code 1))
    ;; Run under lldb
    (let* ((cmd (format nil "echo '~A' | timeout 10 lldb -b -o 'run' -o 'bt 10' -o 'register read x0 x1 x20 x27 x28 pc lr sp' '~A' 2>&1"
                        stdin-input binary))
           (output (run-cmd cmd)))
      (format t "~A~%" output))))

(main)
(sb-ext:exit :code 0)
