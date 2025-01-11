(in-package #:lem-lister)

(defun lister-class-to-view-name (lister-class-name)
  "Convert lister class name to view name (e.g. dir-lister -> dir-view)"
  (let* ((class-name-string (symbol-name lister-class-name))
         (base-name (string-trim "LISTER" class-name-string)))
    (intern (concatenate 'string base-name "VIEW"))))

(defmacro define-lister (name superclass &body initargs)
  (let ((view-name (intern (concatenate 'string
                                        (string-trim "LISTER" (symbol-name name))
                                        "VIEW"))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (progn
         (defclass ,name (,superclass) ()
           ,@initargs)
         (define-lister-view ',view-name)))))

(define-lister dir-lister lister
  (:default-initargs
   :columns (list
             (make-instance 'string-column :name "Name")
             (make-instance 'dir-column :name "Path" :display-p t))))

(defmethod initialize-instance :after ((instance lister) &key &allow-other-keys)
  (let ((view-name (lister-class-to-view-name (type-of instance))))
    (eval `(define-lister-view ,view-name))))
