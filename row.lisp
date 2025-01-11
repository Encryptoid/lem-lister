(in-package #:lem-lister)

(defclass lister-row ()
  ((values
    :initarg :values
    :accessor row-values
    :initform (make-hash-table)  ; stores column -> value mappings
    :documentation "Hash table mapping columns to their values")))

(defun make-lister-row (column-values)
  "Create a row from a list of values in column order"
  (let ((row (make-instance 'lister-row)))
    (setf (row-values row)
          (make-hash-table))  ; Initialize hash table for values
    row))

(defun set-row-values (row lister values)
  "Set values for a row based on lister's columns"
  (let ((columns (lister-columns lister)))
    (when (/= (length columns) (length values))
      (error "Number of values (~A) doesn't match number of columns (~A)"
             (length values) (length columns)))
    (loop for column in columns
          for value in values
          do (setf (gethash column (row-values row)) value))
    row))

(defun add-new-row (lister &rest values)
  "Add a new row to a lister with the given values"
  (let ((row (make-lister-row values)))
    (set-row-values row lister values)
    (push row (lister-rows lister))
    row))
