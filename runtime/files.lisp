;;;; Habu Runtime - File I/O Operations
;;;; Basic file operations for reading and writing

(in-package :habu-runtime)

(export '(runtime-file-open
          runtime-file-read
          runtime-file-write
          runtime-file-close
          runtime-read-file
          runtime-write-file))

;;; File handle table
;;; Maps Habu file handles (tagged fixnums) to Lisp streams
(defvar *file-handle-table* (make-hash-table))
(defvar *next-file-handle* 1)

(defun allocate-file-handle (stream)
  "Allocate a new file handle for a stream"
  (let ((handle *next-file-handle*))
    (incf *next-file-handle*)
    (setf (gethash handle *file-handle-table*) stream)
    ;; Return as tagged fixnum
    (ash handle 4)))

(defun get-file-stream (handle-fixnum)
  "Get stream from tagged file handle"
  (let ((handle (ash handle-fixnum -4)))
    (gethash handle *file-handle-table*)))

(defun free-file-handle (handle-fixnum)
  "Free a file handle"
  (let ((handle (ash handle-fixnum -4)))
    (remhash handle *file-handle-table*)))

;;; File operations

(defun runtime-file-open (path-ptr mode-ptr)
  "Open a file and return a file handle (tagged fixnum)
   path-ptr: Habu string pointer (path to file)
   mode-ptr: Habu string pointer (\"r\" for read, \"w\" for write, \"a\" for append)
   Returns: Tagged fixnum file handle, or 0 on error"
  (unless (= (logand path-ptr #xF) +tag-string+)
    (error "Not a string: ~X" path-ptr))
  (unless (= (logand mode-ptr #xF) +tag-string+)
    (error "Not a string: ~X" mode-ptr))

  (let* ((path (runtime-string->lisp path-ptr))
         (mode-str (runtime-string->lisp mode-ptr))
         (direction (cond
                      ((string= mode-str "r") :input)
                      ((string= mode-str "w") :output)
                      ((string= mode-str "a") :output)
                      (t (error "Invalid mode: ~S" mode-str))))
         (if-exists (if (string= mode-str "a") :append :supersede))
         (if-does-not-exist (if (string= mode-str "r") nil :create)))

    (handler-case
        (let ((stream (open path
                           :direction direction
                           :if-exists if-exists
                           :if-does-not-exist if-does-not-exist
                           :element-type 'character)))
          (if stream
              (allocate-file-handle stream)
              0))  ; Return 0 (NIL) on error
      (error (e)
        (format *error-output* "Error opening file ~S: ~A~%" path e)
        0))))

(defun runtime-file-read (handle-fixnum)
  "Read entire contents from file handle
   handle-fixnum: Tagged fixnum file handle
   Returns: Habu string pointer with contents, or 0 on error"
  (let ((stream (get-file-stream handle-fixnum)))
    (unless stream
      (error "Invalid file handle: ~X" handle-fixnum))

    (handler-case
        (let* ((contents (make-array 0 :element-type 'character
                                      :fill-pointer 0
                                      :adjustable t)))
          (loop for char = (read-char stream nil nil)
                while char
                do (vector-push-extend char contents))
          (runtime-lisp->string contents))
      (error (e)
        (format *error-output* "Error reading file: ~A~%" e)
        0))))

(defun runtime-file-write (handle-fixnum data-ptr)
  "Write string to file handle
   handle-fixnum: Tagged fixnum file handle
   data-ptr: Habu string pointer with data to write
   Returns: Tagged fixnum number of bytes written, or 0 on error"
  (unless (= (logand data-ptr #xF) +tag-string+)
    (error "Not a string: ~X" data-ptr))

  (let ((stream (get-file-stream handle-fixnum)))
    (unless stream
      (error "Invalid file handle: ~X" handle-fixnum))

    (handler-case
        (let ((data (runtime-string->lisp data-ptr)))
          (write-string data stream)
          (force-output stream)
          ;; Return length as tagged fixnum
          (ash (length data) 4))
      (error (e)
        (format *error-output* "Error writing file: ~A~%" e)
        0))))

(defun runtime-file-close (handle-fixnum)
  "Close a file handle
   handle-fixnum: Tagged fixnum file handle
   Returns: Tagged fixnum 1 on success, 0 on error"
  (let ((stream (get-file-stream handle-fixnum)))
    (unless stream
      (error "Invalid file handle: ~X" handle-fixnum))

    (handler-case
        (progn
          (close stream)
          (free-file-handle handle-fixnum)
          ;; Return 1 (true) as tagged fixnum
          #x10)
      (error (e)
        (format *error-output* "Error closing file: ~A~%" e)
        0))))

;;; Convenience functions

(defun runtime-read-file (path-ptr)
  "Read entire file contents as a string
   path-ptr: Habu string pointer (path to file)
   Returns: Habu string pointer with contents, or 0 on error"
  (let* ((mode-str (runtime-lisp->string "r"))
         (handle (runtime-file-open path-ptr mode-str)))
    (if (zerop handle)
        0  ; Error opening file
        (let ((contents (runtime-file-read handle)))
          (runtime-file-close handle)
          contents))))

(defun runtime-write-file (path-ptr data-ptr)
  "Write string to file (overwrite if exists)
   path-ptr: Habu string pointer (path to file)
   data-ptr: Habu string pointer (data to write)
   Returns: Tagged fixnum 1 on success, 0 on error"
  (let* ((mode-str (runtime-lisp->string "w"))
         (handle (runtime-file-open path-ptr mode-str)))
    (if (zerop handle)
        0  ; Error opening file
        (let ((bytes-written (runtime-file-write handle data-ptr)))
          (runtime-file-close handle)
          (if (zerop bytes-written)
              0
              #x10)))))  ; Return 1 (true) as tagged fixnum
