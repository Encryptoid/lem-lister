(in-package #:lem-lister)

(defclass lister ()
  ((name
    :initarg :name
    :accessor lister-name
    :initform (error "Must provide lister name"))
   (columns
    :initarg :columns
    :accessor lister-columns)
   (rows
    :initarg :rows
    :accessor lister-rows
    :initform nil)
   ))

(defparameter *listers* (make-hash-table :test 'equal))
(defun register-lister (lister)
  "Register a lister in the global table"
  (setf (gethash (lister-name lister) *listers*) lister))

(defun get-lister (name)
  "Get a lister by name"
  (gethash name *listers*))


(defun print-row (row lister)
  "Print a row's values with their column names"
  (loop for column in (lister-columns lister)
        do (format t "~A: ~A~%"
                   (field-name column)
                   (gethash column (row-values row)))))

(defun print-lister (lister)
  (format t "Lister - Name: ~a, Rows: ~a~%" (lister-name lister) (length (lister-rows lister)))
  (dolist (row (lister-rows lister))
    (print-row row lister)
    (format t "~%")))

;(print-lister (get-lister "Workspaces"))
