(defpackage #:lem-lister
  (:use #:cl #:lem))

(in-package #:lem-lister)

;; Column
(defclass list-column ()
  ((name
    :initarg :name
    :accessor column-name)
   (display-p
    :initarg :display-p
    :accessor column-display-p
    :initform t)
   (formatter
    :initarg :formatter
    :accessor field-formatter
    :initform #'column-formatter)
   (new
    :initarg :new
    :accessor column-new)))

(defun column-formatter (col)
  col)

;; ROW

(defun add-new-row (lister &rest values)
  "Add a new row to a lister with the given values"
  (let* ((current-rows (read-rows-from-file lister))
         (row-alist (make-row-alist lister values)))
    (write-rows-to-file lister (cons row-alist current-rows))
    row-alist))

(defun read-rows-from-file (lister)
  "Read all rows from the lister's file"
  (with-open-file (stream (lister-filename lister)
                          :direction :input
                          :if-does-not-exist :create)
    (or (ignore-errors (read stream)) nil)))

(defun make-row-alist (lister values)
  "Create an alist mapping column names to values"
  (let ((columns (lister-columns lister)))
    (when (/= (length columns) (length values))
      (error "Number of values (~A) doesn't match number of columns (~A)"
             (length values) (length columns)))
    (loop for column in columns
          for value in values
          collect (cons (column-name column) value))))

(defun write-rows-to-file (lister rows)
  "Write all rows to the lister's file"
  (with-open-file (stream (lister-filename lister)
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (prin1 rows stream)))

(defun remove-row (lister row)
  "Remove a specific row from a lister's stored rows"
  (let ((current-rows (read-rows-from-file lister)))
    (write-rows-to-file lister (remove row current-rows :test #'equal))
    (values)))

;; LIST
(defclass lister ()
  ((name
    :initarg :name
    :accessor lister-name
    :initform (error "Must provide lister name"))
   (columns
    :initarg :columns
    :accessor lister-columns)
   (filename
    :initarg :filename
    :accessor lister-filename)
   (handler
    :initarg :handler
    :accessor lister-handler
    :initform nil)
   ))

(defparameter *lister-dir* "/home/l/.local/share/lem/lister/")

(defmethod initialize-instance :after ((lister lister) &key &allow-other-keys)
  "Initialize the lister's storage file"
  (setf (lister-filename lister)
        (merge-pathnames
         *lister-dir*
         (format nil "lister-~A.dat" (lister-name lister))
         ))
  (unless (probe-file (lister-filename lister))
    (write-rows-to-file lister nil))
  (register-lister lister))

(defparameter *listers* (make-hash-table :test 'equal))
(defun register-lister (lister)
  "Register a lister in the global table"
  (setf (gethash (lister-name lister) *listers*) lister))

(defun get-lister (name)
  "Get a lister by name"
  (gethash name *listers*))

(defmacro define-template (name &body initargs)
  `(defclass ,name (lister) ()
     ,@initargs))

(defmacro define-lister-command (template name &optional key)
  (let ((cmd (intern (string-upcase (format nil "LISTER-~A" name)))))
    `(progn
       (make-instance ,template :name ,name)
       (lem:define-command ,cmd () ()
         (display-lister-view (get-lister ,name)))
       (if ,key
         (define-key *global-keymap* ,key ',cmd))
       )))
