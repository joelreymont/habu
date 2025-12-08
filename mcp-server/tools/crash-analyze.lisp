;;;; crash-analyze.lisp - Comprehensive crash analysis tool
;;;;
;;;; Runs binary under lldb, captures crash info, analyzes registers,
;;;; heap state, decodes tagged values, and provides diagnosis.

(in-package :habu-mcp)

(defun parse-hex (str)
  "Parse hex value from string, handling 0x prefix."
  (when (and str (stringp str) (> (length str) 0))
    (handler-case
        (parse-integer (string-trim '(#\Space #\Tab)
                                    (if (and (> (length str) 2)
                                             (string= (subseq str 0 2) "0x"))
                                        (subseq str 2)
                                        str))
                       :radix 16)
      (error () nil))))

(defun extract-reg-value (line reg-name)
  "Extract register value from lldb output line."
  (let ((pos (search reg-name line)))
    (when pos
      (let ((hex-pos (search "0x" line :start2 pos)))
        (when hex-pos
          (let ((end (position-if-not
                      (lambda (c) (or (digit-char-p c 16) (char= c #\x)))
                      line :start (+ hex-pos 2))))
            (subseq line hex-pos (or end (length line)))))))))

(defun decode-tag (value)
  "Decode Habu tagged value."
  (when (and value (integerp value))
    (cond
      ((= value #x06) "NIL")
      ((= value #x10) "T")
      ((= (logand value #xF) 0) (format nil "FIXNUM(~D)" (ash value -4)))
      ((= (logand value #xF) 1) "CONS")
      ((= (logand value #xF) 2) "SYMBOL")
      ((= (logand value #xF) 3) "VECTOR")
      ((= (logand value #xF) 4) "STRING")
      ((= (logand value #xF) 5) "CLOSURE")
      ((= (logand value #xF) 7) "FORWARDING-PTR!")
      (t (format nil "TAG-~D" (logand value #xF))))))

(defun resolve-symbol (pc map-file)
  "Resolve PC to symbol using .map file."
  (handler-case
      (when (and pc (probe-file map-file))
        (with-open-file (f map-file)
          (let ((best-sym nil) (best-addr 0))
            (loop for line = (read-line f nil nil) while line do
              (when (and (> (length line) 2)
                         (char= (char line 0) #\0)
                         (char= (char line 1) #\x))
                (let* ((space (position #\Space line))
                       (addr (when space (parse-hex (subseq line 0 space))))
                       (sym (when space (string-trim " " (subseq line space)))))
                  (when (and addr (<= addr pc) (> addr best-addr))
                    (setf best-sym sym best-addr addr)))))
            (when best-sym
              (format nil "~A+~D" best-sym (- pc best-addr))))))
    (error () nil)))

(defun analyze-cause (x0 x1 x12 x27 x28)
  "Analyze likely crash cause from registers."
  (let ((causes nil))
    ;; Nil/zero dereference
    (when (and x0 (< x0 #x1000))
      (push (format nil "Dereferencing ~A (x0=0x~X)"
                    (if (= x0 6) "NIL" "near-zero") x0) causes))
    (when (and x12 (< x12 #x1000))
      (push (format nil "Dereferencing ~A (x12=0x~X)"
                    (if (= x12 6) "NIL" "near-zero") x12) causes))
    ;; Forwarding pointer (stale GC ref)
    (when (and x0 (= (logand x0 #xF) 7))
      (push "Stale GC pointer in x0 (forwarding ptr)" causes))
    (when (and x1 (= (logand x1 #xF) 7))
      (push "Stale GC pointer in x1 (forwarding ptr)" causes))
    ;; Heap overflow
    (when (and x27 x28)
      (let ((from-end (+ x27 96 #x4000000)))
        (when (>= x28 from-end)
          (push (format nil "Heap overflow (x28 past from-end)") causes))))
    (or causes (list "Unknown cause"))))

(defun tool-crash-analyze (args)
  "Comprehensive crash analysis: run binary, capture all crash info, analyze."
  (let* ((binary (jget args "binary"))
         (cmd-args (or (jget args "args") ""))
         (timeout (or (jget args "timeout") 30))
         (map-file (format nil "~A.map" (subseq binary 0 (or (position #\. binary :from-end t)
                                                             (length binary))))))
    (handler-case
        (let* ((lldb-args (if (string= cmd-args "")
                              (list binary "-o" "run")
                              (list binary "-o" (format nil "run ~A" cmd-args))))
               (proc (sb-ext:run-program "/usr/bin/lldb"
                                         (append lldb-args
                                                 (list "-o" "register read"
                                                       "-o" "disassemble -p -c 15"
                                                       "-o" "bt 10"
                                                       "-o" "quit"))
                                         :input nil :output :stream :error :stream :wait nil))
               (lines nil)
               (lock (sb-thread:make-mutex)))
          (let ((thread (sb-thread:make-thread
                         (lambda ()
                           (ignore-errors
                             (loop for line = (read-line (sb-ext:process-output proc) nil nil)
                                   while line do
                                   (sb-thread:with-mutex (lock) (push line lines))))))))
            ;; Wait with timeout
            (let ((start (get-internal-real-time)) (killed nil))
              (loop
                (unless (sb-ext:process-alive-p proc) (return))
                (when (> (/ (- (get-internal-real-time) start) internal-time-units-per-second) timeout)
                  (sb-ext:process-kill proc 9) (setf killed t) (sleep 0.1) (return))
                (sleep 0.1))
              (sb-ext:process-wait proc)
              (sleep 0.2)
              (when (sb-thread:thread-alive-p thread)
                (ignore-errors (sb-thread:terminate-thread thread)))
              ;; Parse output
              (let* ((all-lines (sb-thread:with-mutex (lock) (nreverse lines)))
                     (regs (make-hash-table :test 'equal))
                     (stop-reason nil) (disasm nil) (bt nil)
                     (in-regs nil) (in-disasm nil) (in-bt nil))
                (dolist (line all-lines)
                  (cond
                    ((search "stop reason" line) (setf stop-reason line))
                    ((search "General Purpose" line) (setf in-regs t in-disasm nil in-bt nil))
                    ((or (search "->" line) (and in-disasm (search "0x" line)))
                     (setf in-disasm t in-regs nil) (push line disasm))
                    ((search "frame #" line) (setf in-bt t in-regs nil in-disasm nil) (push line bt))
                    (in-regs
                     (dolist (r '("x0" "x1" "x2" "x9" "x10" "x12" "x20" "x27" "x28" "pc" "sp" "lr"))
                       (let ((v (extract-reg-value line r)))
                         (when v (setf (gethash r regs) v)))))))
                ;; Extract values
                (let* ((x0 (parse-hex (gethash "x0" regs)))
                       (x1 (parse-hex (gethash "x1" regs)))
                       (x12 (parse-hex (gethash "x12" regs)))
                       (x20 (parse-hex (gethash "x20" regs)))
                       (x27 (parse-hex (gethash "x27" regs)))
                       (x28 (parse-hex (gethash "x28" regs)))
                       (pc (parse-hex (gethash "pc" regs)))
                       (sym (when pc (resolve-symbol pc map-file)))
                       (causes (analyze-cause x0 x1 x12 x27 x28)))
                  (with-output-to-string (out)
                    (format out "=== CRASH ANALYSIS ===~%~%")
                    (when killed (format out "[TIMEOUT]~%"))
                    ;; Stop reason
                    (when stop-reason
                      (format out "~A~%~%" stop-reason))
                    ;; Location
                    (format out "Location: ~A~%" (or (gethash "pc" regs) "?"))
                    (when sym (format out "Symbol:   ~A~%~%" sym))
                    ;; Likely cause
                    (format out "Likely Cause:~%")
                    (dolist (c causes) (format out "  - ~A~%" c))
                    (format out "~%")
                    ;; Key registers with decoded values
                    (format out "Registers:~%")
                    (dolist (r '("x0" "x1" "x12" "x20" "x27" "x28" "pc"))
                      (let* ((v (gethash r regs))
                             (n (parse-hex v))
                             (d (when (member r '("x0" "x1" "x12") :test #'string=)
                                  (decode-tag n))))
                        (when v
                          (format out "  ~3A = ~A~@[ [~A]~]~%" r v d))))
                    (format out "~%")
                    ;; Heap state
                    (when (and x27 x28)
                      (let* ((from-start (+ x27 96))
                             (used (- x28 from-start))
                             (half #x4000000))
                        (format out "Heap: ~:D / ~:D bytes (~,1F% used)~%~%"
                                used half (* 100.0 (/ used half)))))
                    ;; Disassembly
                    (when disasm
                      (format out "Disassembly:~%")
                      (dolist (l (reverse (subseq disasm 0 (min 8 (length disasm)))))
                        (format out "  ~A~%" l))
                      (format out "~%"))
                    ;; Backtrace
                    (when bt
                      (format out "Backtrace:~%")
                      (dolist (l (reverse (subseq bt 0 (min 5 (length bt)))))
                        (format out "  ~A~%" l))
                      (format out "~%"))
                    ;; Next steps
                    (format out "Next Steps:~%")
                    (cond
                      ((member-if (lambda (c) (search "NIL" c)) causes)
                       (format out "  1. Check nil handling before car/cdr~%")
                       (format out "  2. Verify function returns valid value~%"))
                      ((member-if (lambda (c) (search "forwarding" c)) causes)
                       (format out "  1. GC bug - pointer not updated after collection~%")
                       (format out "  2. Check env slots used after GC~%"))
                      (t
                       (format out "  1. Examine registers and disassembly~%")
                       (format out "  2. Step through with lldb~%")))))))))
      (error (e)
        (format nil "Error: ~A" e)))))

;;; Tool registration entry (add to *tools* in mcp.lisp):
;;; ("crash-analyze"
;;;  "Comprehensive crash analysis: runs binary, captures registers, disassembly, backtrace, analyzes heap, decodes tags, suggests causes."
;;;  (("binary" "string" "Path to binary" t)
;;;   ("args" "string" "Command line args (optional)" nil)
;;;   ("timeout" "number" "Timeout seconds (default 30)" nil))
;;;  tool-crash-analyze)
