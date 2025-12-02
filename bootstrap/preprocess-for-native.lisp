;;; Preprocessor to create native-mode source files
;;; Strips #+sbcl forms, keeps #-sbcl contents

(defun preprocess-source (input-path output-path)
  "Process a Lisp source file for native mode.
   - Lines starting with #+sbcl: skip until balanced parentheses
   - Lines starting with #-sbcl: remove the #-sbcl prefix, keep content
   - Other lines: keep as-is"
  (with-open-file (in input-path :direction :input)
    (with-open-file (out output-path :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
      (let ((skip-depth 0)
            (in-plus-sbcl nil))
        (loop for line = (read-line in nil nil)
              while line
              do
              (cond
                ;; Start of #+sbcl block
                ((and (>= (length line) 6)
                      (string= (subseq line 0 6) "#+sbcl"))
                 (setf in-plus-sbcl t)
                 (setf skip-depth (count-open-parens line)))
                
                ;; Currently skipping #+sbcl block
                (in-plus-sbcl
                 (incf skip-depth (count-open-parens line))
                 (when (<= skip-depth 0)
                   (setf in-plus-sbcl nil)
                   (setf skip-depth 0)))
                
                ;; #-sbcl line - strip prefix, keep content
                ((and (>= (length line) 6)
                      (string= (subseq line 0 6) "#-sbcl"))
                 (let ((rest (subseq line 6)))
                   ;; Remove leading space if present
                   (when (and (> (length rest) 0)
                              (char= (char rest 0) #\Space))
                     (setf rest (subseq rest 1)))
                   (write-line rest out)))
                
                ;; Normal line - keep as-is
                (t
                 (write-line line out))))))))

(defun count-open-parens (line)
  "Count net open parentheses in a line"
  (let ((count 0))
    (loop for c across line
          do (cond ((char= c #\() (incf count))
                   ((char= c #\)) (decf count))))
    count))

(defun preprocess-all ()
  "Preprocess all bootstrap files for native mode"
  (let ((files '("reader" "compiler" "optimize" "codegen" "gc" "macho-utils")))
    (dolist (f files)
      (let ((input (format nil "bootstrap/~A.lisp" f))
            (output (format nil "/tmp/native-~A.lisp" f)))
        (format t "Processing ~A -> ~A~%" input output)
        (preprocess-source input output)))
    (format t "Done. Native sources in /tmp/native-*.lisp~%")))
