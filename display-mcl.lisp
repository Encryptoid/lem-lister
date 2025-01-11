(in-package #:lem-lister)

(defmacro define-lister-view (name &body body)
  (let ((item-class-name (intern (format nil "~A-ITEM" name)))
        (menu-class-name (intern (format nil "~A-MENU" name))))
    `(progn
       ;; Item class definition
       (defclass ,item-class-name (lem/multi-column-list:multi-column-list-item)
         ((lister-row :initarg :row
                      :reader lister-row)))

       ;; Add method to handle value extraction for the wrapper system
       (defmethod lem/multi-column-list::default-multi-column-list-item-value ((item ,item-class-name))
         (lister-row item))

       ;; Menu class definition
       (defclass ,menu-class-name (lem/multi-column-list:multi-column-list)
         ((source-lister :initarg :source-lister
                         :reader source-lister)))

       ;; Initialize columns and filter function
       (defmethod initialize-instance :after ((menu ,menu-class-name) &key source-lister &allow-other-keys)
         (setf (slot-value menu 'lem/multi-column-list::columns)
               (mapcar #'field-name
                       (remove-if-not #'field-display-p
                                      (lister-columns source-lister))))

         (setf (slot-value menu 'lem/multi-column-list::filter-function)
               (lambda (search-string)
                 (let ((search-str (string-downcase search-string)))
                   (remove-if-not
                    (lambda (item)
                      (let* ((row (lister-row item))
                             (displayed-columns (remove-if-not #'field-display-p
                                                               (lister-columns (source-lister menu))))
                             (values (mapcar (lambda (col)
                                               (princ-to-string
                                                (gethash col (row-values row))))
                                             displayed-columns)))
                        (some (lambda (value)
                                (search search-str
                                        (string-downcase value)))
                              values)))
                    (slot-value menu 'lem/multi-column-list::items))))))

       ;; Map columns method
       (defmethod lem/multi-column-list:map-columns ((component ,menu-class-name)
                                                     (item ,item-class-name))
         (let* ((row (lister-row item))
                (displayed-columns (remove-if-not #'field-display-p
                                                  (lister-columns (source-lister component)))))
           (mapcar (lambda (col)
                     (princ-to-string
                      (gethash col (row-values row))))
                   displayed-columns)))

       ;; Select item method
       (defmethod lem/multi-column-list:select-item ((component ,menu-class-name) item)
         (lem:message "selected")
         (lem/multi-column-list:update component)))));; Function to display a lister




;; ;; Enhanced display function that determines view automatically
;; (defun display-lister-view (lister)
;;   "Display a lister in the multi-column view, automatically determining the view type.
;;    The view type is derived from the lister's class name by replacing '-lister' with '-view'."
;;   (let* ((lister-type (type-of lister))
;;          (view-name (lister-class-to-view-name lister-type))
;;          (menu-class-name (intern (format nil "~A-MENU" view-name))))
;;     ;; Check if the view exists
;;     (unless (find-class menu-class-name nil)
;;       (error "No view defined for lister type ~A. Expected view ~A"
;;              lister-type view-name))
;;     ;; Display using the determined view class
;;     (lem/multi-column-list:display
;;      (make-instance menu-class-name
;;                     :source-lister lister
;;                     :items (mapcar (lambda (row)
;;                                      (make-instance (intern (format nil "~A-ITEM" view-name))
;;                                                     :row row))
;;                                    (lister-rows lister))))))

(defun ensure-view-exists (view-name)
  "Make sure view is defined, return t if newly created"
  (let ((menu-class-name (intern (format nil "~A-MENU" view-name))))
    (when (not (find-class menu-class-name nil))
      (define-lister-view view-name)
      t)))

(defun display-lister-view (lister)
  (let* ((view-name (lister-class-to-view-name (type-of lister)))
         (menu-class-name (intern (format nil "~A-MENU" view-name))))
    ;; Try to ensure view exists first
    (ensure-view-exists view-name)
    ;; Now display
    (lem/multi-column-list:display
     (make-instance menu-class-name
                    :source-lister lister
                    :items (mapcar (lambda (row)
                                     (make-instance (intern (format nil "~A-ITEM" view-name))
                                                    :row row))
                                   (lister-rows lister))))))

(defmacro define-lister (name superclass &body initargs)
  `(defclass ,name (,superclass) ()
     ,@initargs))
