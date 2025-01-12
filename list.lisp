(in-package #:lem-lister)

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
    :accessor field-handler
    :initform nil)
   ))

(defmethod initialize-instance :after ((menu lem/multi-column-list:multi-column-list) 
                                       &key source-lister &allow-other-keys)
  (when source-lister
    (setf (slot-value menu 'source-lister) source-lister)
    (setf (slot-value menu 'lem/multi-column-list::columns)
          (mapcar #'field-name
                  (remove-if-not #'field-display-p
                                 (lister-columns source-lister))))
    (setf (slot-value menu 'lem/multi-column-list::select-callback)
          (lambda (menu item)
            (declare (ignore menu))
            (when-let ((handler (lister-handler source-lister)))
              (funcall handler (lem/multi-column-list::unwrap 
                                (lem/multi-column-list::default-multi-column-list-item-value item))))))
    (setf (slot-value menu 'lem/multi-column-list::filter-function)
          nil)))

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
