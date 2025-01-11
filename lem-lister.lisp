(defpackage #:lem-lister
  (:use #:cl))
(in-package #:lem-lister)

(defclass list-column ()
  ((name
    :initarg :name
    :accessor field-name)
   (handler
    :initarg :handler
    :accessor field-handler)
   (display-p
    :initarg :display-p
    :accessor field-display-p
    :initform t)
   (formatter
    :initarg :formatter
    :accessor field-formatter
    :initform #'format-list-column)))

(defun format-list-column (col)
  col)

;; String Column
(defclass string-column (list-column)
  ()
  (:default-initargs
   :handler #'handle-string-column
   :formatter #'format-string-column))

(defun format-string-column (col)
  col)

(defun handle-string-column (col)
  ;; To Clip
  )

;; Dir Column
(defclass dir-column (list-column) ()
  (:default-initargs
   :handler #'handle-dir-column
   ;; :formatter #'format-dir-column
   ))

(defun format-dir-column (col)
  col)

(defun handle-dir-column (col)
  ;; To Clip
)

;; (setq c (make-instance 'string-column :name "hi"))
;; (format nil "~a" (funcall (field-formatter c) "a"))
