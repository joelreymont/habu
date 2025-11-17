;;;; Simple readline implementation for Habu REPL
;;;; Provides line editing, history navigation, and tab completion

(in-package :habu-compiler)

;;; Terminal control codes
(defparameter *esc* (code-char 27))
(defparameter *ctrl-a* (code-char 1))
(defparameter *ctrl-e* (code-char 5))
(defparameter *ctrl-d* (code-char 4))
(defparameter *ctrl-k* (code-char 11))
(defparameter *ctrl-u* (code-char 21))
(defparameter *ctrl-l* (code-char 12))
(defparameter *backspace* (code-char 127))
(defparameter *tab* (code-char 9))
(defparameter *newline* (code-char 10))
(defparameter *return* (code-char 13))

;;; Line editor state
(defstruct line-editor
  (buffer "" :type string)
  (cursor 0 :type fixnum)
  (history nil :type list)
  (history-pos -1 :type fixnum)
  (original-line "" :type string)
  (completion-fn nil :type (or null function)))

(defun clear-line ()
  "Clear the current line"
  (format t "~C[2K~C[G" *esc* *esc*)
  (force-output))

(defun move-cursor-to (pos)
  "Move cursor to absolute position"
  (format t "~C[G~C[~DC" *esc* *esc* (1+ pos))
  (force-output))

(defun redisplay-line (editor prompt)
  "Redisplay the current line with cursor at correct position"
  (clear-line)
  (format t "~A~A" prompt (line-editor-buffer editor))
  (move-cursor-to (+ (length prompt) (line-editor-cursor editor)))
  (force-output))

(defun insert-char (editor ch)
  "Insert character at cursor position"
  (let* ((buf (line-editor-buffer editor))
         (pos (line-editor-cursor editor))
         (new-buf (concatenate 'string
                              (subseq buf 0 pos)
                              (string ch)
                              (subseq buf pos))))
    (setf (line-editor-buffer editor) new-buf)
    (incf (line-editor-cursor editor))))

(defun delete-char (editor)
  "Delete character at cursor position"
  (let* ((buf (line-editor-buffer editor))
         (pos (line-editor-cursor editor)))
    (when (< pos (length buf))
      (setf (line-editor-buffer editor)
            (concatenate 'string
                        (subseq buf 0 pos)
                        (subseq buf (1+ pos)))))))

(defun backward-delete-char (editor)
  "Delete character before cursor"
  (when (> (line-editor-cursor editor) 0)
    (decf (line-editor-cursor editor))
    (delete-char editor)))

(defun move-beginning (editor)
  "Move cursor to beginning of line"
  (setf (line-editor-cursor editor) 0))

(defun move-end (editor)
  "Move cursor to end of line"
  (setf (line-editor-cursor editor) (length (line-editor-buffer editor))))

(defun move-forward (editor)
  "Move cursor forward one character"
  (when (< (line-editor-cursor editor) (length (line-editor-buffer editor)))
    (incf (line-editor-cursor editor))))

(defun move-backward (editor)
  "Move cursor backward one character"
  (when (> (line-editor-cursor editor) 0)
    (decf (line-editor-cursor editor))))

(defun kill-to-end (editor)
  "Kill from cursor to end of line"
  (setf (line-editor-buffer editor)
        (subseq (line-editor-buffer editor) 0 (line-editor-cursor editor))))

(defun kill-to-beginning (editor)
  "Kill from beginning to cursor"
  (let ((buf (line-editor-buffer editor))
        (pos (line-editor-cursor editor)))
    (setf (line-editor-buffer editor) (subseq buf pos))
    (setf (line-editor-cursor editor) 0)))

(defun history-previous (editor)
  "Move to previous history entry"
  (let ((hist (line-editor-history editor))
        (pos (line-editor-history-pos editor)))
    (when (< (1+ pos) (length hist))
      ;; Save current line if we're at the bottom
      (when (= pos -1)
        (setf (line-editor-original-line editor) (line-editor-buffer editor)))
      ;; Move up in history
      (incf (line-editor-history-pos editor))
      (setf (line-editor-buffer editor)
            (nth (line-editor-history-pos editor) hist))
      (move-end editor))))

(defun history-next (editor)
  "Move to next history entry"
  (let ((pos (line-editor-history-pos editor)))
    (when (>= pos 0)
      (decf (line-editor-history-pos editor))
      (if (= (line-editor-history-pos editor) -1)
          ;; Back to original line
          (setf (line-editor-buffer editor) (line-editor-original-line editor))
          ;; Next in history
          (setf (line-editor-buffer editor)
                (nth (line-editor-history-pos editor) (line-editor-history editor))))
      (move-end editor))))

(defun try-complete (editor)
  "Try to complete the current word"
  (when (line-editor-completion-fn editor)
    (let* ((buf (line-editor-buffer editor))
           (pos (line-editor-cursor editor))
           ;; Find word start
           (start (or (position-if (lambda (ch) (member ch '(#\Space #\( #\))))
                                   buf :from-end t :end pos)
                      -1))
           (word (subseq buf (1+ start) pos))
           (completions (funcall (line-editor-completion-fn editor) word)))
      (cond
        ((null completions)
         ;; No completions - beep (or do nothing)
         nil)
        ((= (length completions) 1)
         ;; Single completion - insert it
         (let ((completion (first completions))
               (to-insert (subseq (string completion) (length word))))
           (loop for ch across to-insert
                 do (insert-char editor ch))))
        (t
         ;; Multiple completions - show them
         (format t "~%Completions:~%")
         (dolist (c completions)
           (format t "  ~A~%" c))
         (format t "~%"))))))

(defun read-escape-sequence ()
  "Read an escape sequence (arrow keys, etc.)"
  (let ((ch1 (read-char *standard-input* nil nil)))
    (when (and ch1 (char= ch1 #\[))
      (let ((ch2 (read-char *standard-input* nil nil)))
        (case ch2
          (#\A :up)       ; Up arrow
          (#\B :down)     ; Down arrow
          (#\C :right)    ; Right arrow
          (#\D :left)     ; Left arrow
          (#\H :home)     ; Home
          (#\F :end)      ; End
          (otherwise nil))))))

(defun read-line-with-editing (prompt &key history completion-fn)
  "Read a line with editing support"
  (let ((editor (make-line-editor :history history
                                  :completion-fn completion-fn)))
    (format t "~A" prompt)
    (force-output)

    (loop
      (let ((ch (read-char *standard-input* nil nil)))
        (unless ch (return nil))

        (cond
          ;; Newline/Return - done
          ((or (char= ch *newline*) (char= ch *return*))
           (format t "~%")
           (return (line-editor-buffer editor)))

          ;; Escape sequence (arrow keys, etc.)
          ((char= ch *esc*)
           (let ((seq (read-escape-sequence)))
             (case seq
               (:up (history-previous editor))
               (:down (history-next editor))
               (:left (move-backward editor))
               (:right (move-forward editor))
               (:home (move-beginning editor))
               (:end (move-end editor))))
           (redisplay-line editor prompt))

          ;; Ctrl-A - beginning of line
          ((char= ch *ctrl-a*)
           (move-beginning editor)
           (redisplay-line editor prompt))

          ;; Ctrl-E - end of line
          ((char= ch *ctrl-e*)
           (move-end editor)
           (redisplay-line editor prompt))

          ;; Ctrl-K - kill to end
          ((char= ch *ctrl-k*)
           (kill-to-end editor)
           (redisplay-line editor prompt))

          ;; Ctrl-U - kill to beginning
          ((char= ch *ctrl-u*)
           (kill-to-beginning editor)
           (redisplay-line editor prompt))

          ;; Ctrl-L - clear screen
          ((char= ch *ctrl-l*)
           (format t "~C[2J~C[H" *esc* *esc*)  ; Clear screen and home
           (redisplay-line editor prompt))

          ;; Ctrl-D - delete or EOF
          ((char= ch *ctrl-d*)
           (if (zerop (length (line-editor-buffer editor)))
               (return nil)  ; EOF
               (progn
                 (delete-char editor)
                 (redisplay-line editor prompt))))

          ;; Backspace - delete backward
          ((char= ch *backspace*)
           (backward-delete-char editor)
           (redisplay-line editor prompt))

          ;; Tab - completion
          ((char= ch *tab*)
           (try-complete editor)
           (redisplay-line editor prompt))

          ;; Regular character - insert
          ((and (graphic-char-p ch) (standard-char-p ch))
           (insert-char editor ch)
           (redisplay-line editor prompt))

          ;; Ignore other control characters
          (t nil))))))

(defun set-raw-mode ()
  "Set terminal to raw mode for character-by-character input"
  #+sbcl
  (progn
    (sb-ext:run-program "/bin/stty" '("raw" "-echo") :search t :wait t)
    t)
  #-sbcl
  nil)

(defun restore-cooked-mode ()
  "Restore terminal to cooked mode"
  #+sbcl
  (progn
    (sb-ext:run-program "/bin/stty" '("cooked" "echo") :search t :wait t)
    t)
  #-sbcl
  nil)
