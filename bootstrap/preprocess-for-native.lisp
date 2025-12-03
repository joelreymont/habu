;;; Preprocessor to create native-mode source files
;;; Strips #+sbcl forms, keeps #-sbcl contents
;;; Works on character-level to handle inline conditionals

(defun skip-sbcl-form (source pos)
  "Skip past a #+sbcl form starting at pos.
   Returns position after the skipped form."
  ;; Skip #+sbcl
  (let ((i (+ pos 6)))
    ;; Skip whitespace
    (loop while (and (< i (length source))
                     (member (char source i) '(#\Space #\Tab #\Newline)))
          do (incf i))
    ;; Now we need to skip one form
    ;; Forms can be atoms, lists, or reader macros
    (cond
      ;; List: count parens
      ((and (< i (length source))
            (char= (char source i) #\())
       (let ((depth 1))
         (incf i)
         (loop while (and (< i (length source)) (> depth 0))
               do (let ((c (char source i)))
                    (cond
                      ((char= c #\() (incf depth))
                      ((char= c #\)) (decf depth))
                      ;; Handle strings
                      ((char= c #\")
                       (incf i)
                       (loop while (and (< i (length source))
                                        (not (char= (char source i) #\")))
                             do (when (char= (char source i) #\\)
                                  (incf i))  ; skip escaped char
                             (incf i))
                       (when (< i (length source)) (incf i))
                       (decf i))  ; will be incremented below
                      ;; Handle line comments
                      ((char= c #\;)
                       (loop while (and (< i (length source))
                                        (not (char= (char source i) #\Newline)))
                             do (incf i)))))
               (incf i))
         i))
      ;; Quote or other prefix
      ((and (< i (length source))
            (member (char source i) '(#\' #\` #\,)))
       (incf i)
       (when (and (< i (length source))
                  (char= (char source i) #\@))
         (incf i))
       (skip-sbcl-form source (- i 6)))  ; recursively skip the quoted form
      ;; Atom: read until delimiter
      (t
       (loop while (and (< i (length source))
                        (not (member (char source i)
                                     '(#\Space #\Tab #\Newline #\( #\) #\" #\;))))
             do (incf i))
       i))))

(defun extract-sbcl-form (source pos)
  "Extract the form after #-sbcl starting at pos.
   Returns (form-string end-pos)."
  ;; Skip #-sbcl
  (let ((i (+ pos 6)))
    ;; Skip whitespace (but not newline for inline forms)
    (loop while (and (< i (length source))
                     (member (char source i) '(#\Space #\Tab)))
          do (incf i))
    ;; Now we need to read one form
    (let ((start i))
      (cond
        ;; List: count parens
        ((and (< i (length source))
              (char= (char source i) #\())
         (let ((depth 1))
           (incf i)
           (loop while (and (< i (length source)) (> depth 0))
                 do (let ((c (char source i)))
                      (cond
                        ((char= c #\() (incf depth))
                        ((char= c #\)) (decf depth))
                        ;; Handle strings
                        ((char= c #\")
                         (incf i)
                         (loop while (and (< i (length source))
                                          (not (char= (char source i) #\")))
                               do (when (char= (char source i) #\\)
                                    (incf i))  ; skip escaped char
                               (incf i))
                         (when (< i (length source)) (incf i))
                         (decf i))  ; will be incremented below
                        ;; Handle line comments
                        ((char= c #\;)
                         (loop while (and (< i (length source))
                                          (not (char= (char source i) #\Newline)))
                               do (incf i)))))
                 (incf i))
           (list (subseq source start i) i)))
        ;; Atom
        (t
         (loop while (and (< i (length source))
                          (not (member (char source i)
                                       '(#\Space #\Tab #\Newline #\( #\) #\" #\;))))
               do (incf i))
         (list (subseq source start i) i))))))

(defun preprocess-source (input-path output-path)
  "Process a Lisp source file for native mode.
   Handles inline #+sbcl and #-sbcl anywhere in the source."
  (let* ((source (uiop:read-file-string input-path))
         (result (make-array (length source)
                             :element-type 'character
                             :fill-pointer 0
                             :adjustable t))
         (i 0)
         (n (length source)))
    (loop while (< i n)
          do (cond
               ;; Found #+sbcl
               ((and (<= (+ i 6) n)
                     (string= (subseq source i (+ i 6)) "#+sbcl"))
                ;; Skip the whole form
                (setf i (skip-sbcl-form source i)))
               ;; Found #-sbcl
               ((and (<= (+ i 6) n)
                     (string= (subseq source i (+ i 6)) "#-sbcl"))
                ;; Extract and keep the form
                (let ((result-info (extract-sbcl-form source i)))
                  (loop for c across (first result-info)
                        do (vector-push-extend c result))
                  (setf i (second result-info))))
               ;; Regular character
               (t
                (vector-push-extend (char source i) result)
                (incf i))))
    ;; Write result
    (with-open-file (out output-path :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
      (write-string result out))))

(defun preprocess-all ()
  "Preprocess all bootstrap files for native mode.
   Note: gc.lisp is NOT included - it's SBCL-only code that generates
   GC runtime bytes which get baked into binaries by deliver."
  (let ((files '("reader" "compiler" "optimize" "codegen" "macho-utils" "macho")))
    (dolist (f files)
      (let ((input (format nil "bootstrap/~A.lisp" f))
            (output (format nil "/tmp/native-~A.lisp" f)))
        (format t "Processing ~A -> ~A~%" input output)
        (preprocess-source input output)))
    (format t "Done. Native sources in /tmp/native-*.lisp~%")))
