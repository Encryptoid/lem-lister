(in-package #:lem-lister)


(defclass lister-menu (lem/multi-column-list:multi-column-list)
  ((source-lister :initarg :source-lister
                  :reader lister-menu-source-lister)))

;; Method to convert raw data to columns for display
(defmethod lem/multi-column-list:map-columns ((component lister-menu) data)
  (let* ((lister (lister-menu-source-lister component))
         (columns (lister-columns lister))
         (row-data (typecase data
                     (lem/multi-column-list::default-multi-column-list-item
                         (lem/multi-column-list::default-multi-column-list-item-value data))
                     (t data))))
    (loop :for column :in columns
          :when (column-display-p column)
          :collect (let ((value (cdr (assoc (column-name column) row-data :test #'string=))))
                     (funcall (field-formatter column)
                              (or value ""))))))

;; Primary method for handling raw data
;; In all.lisp
(defmethod lem/multi-column-list:select-item ((component lister-menu) (raw-data cons))
  (let* ((lister (lister-menu-source-lister component))
         (handler (lister-handler lister)))
    (logm:logm (format nil "select-item called with data: ~A" raw-data))
    (handler-case
        (progn
          (lem/multi-column-list:quit component)
          (when handler
            (logm:logm (format nil "About to call handler..."))
            (apply handler (list raw-data)))
          )
      (error (c)
        (logm:logm (format nil "Error in select-item: ~A" c))))))

;; Around method for unwrapping default-multi-column-list-item
(defmethod lem/multi-column-list:select-item :around
    ((component lister-menu) (item lem/multi-column-list::default-multi-column-list-item))
  (handler-case
      (lem/multi-column-list:select-item
       component
       (lem/multi-column-list::default-multi-column-list-item-value item))
    (error (c)
      (logm:logm (format nil "Error in select-item :around method: ~A" c)))))

;; Helper function to get column headers
(defun get-column-headers (lister)
  (loop :for column :in (lister-columns lister)
        :when (column-display-p column)
        :collect (column-name column)))

;; Main display function
(defun filter-lister-items (search-string item)
  "Filter function for lister items based on search string"
  (let ((item-values (typecase item
                       (lem/multi-column-list::default-multi-column-list-item
                           (lem/multi-column-list::default-multi-column-list-item-value item))
                       (t item))))
    (some (lambda (pair)
            (search search-string (string-downcase (format nil "~a" (cdr pair)))
                    :test #'char-equal))
          item-values)))

;; Main display function with search support
(defun display-lister-view (lister)
  (handler-case
      (let ((items (or (read-rows-from-file lister) nil)))
        (lem/multi-column-list:display
         (make-instance 'lister-menu
                        :source-lister lister
                        :columns (get-column-headers lister)
                        :items items
                        :filter-function (lambda (search-string)
                                           (remove-if-not
                                            (lambda (item)
                                              (filter-lister-items search-string item))
                                            items)))
         :style '(:gravity :center)))
    (error (c)
      (logm:logm (format nil "Error in display-lister-view: ~A" c)))))

(defun get-current-lister ()
  (let* ((window (current-window))
         (mcl (lem/multi-column-list:multi-column-list-of-window window)))
    (when (typep mcl 'lister-menu)
      (lister-menu-source-lister mcl))))

;; Helper to create new row values using :new functions
(defun create-new-row-values (lister)
  (loop for column in (lister-columns lister)
        collect (progn
                  (logm:logm "Hiel: ~a" (column-new column))
                  (if (slot-boundp column 'new)
                      (let ((col-new (column-new column)))
                          (funcall col-new column)
                      ;; (dir-column-new column)
                    )))))

;; Command for creating a new row
(define-command lister-menu/new-row () ()
  (let ((current-lister (get-current-lister)))
    (if current-lister
        (progn
        (handler-case
          (let ((new-values (create-new-row-values current-lister)))
            (logm:logm "Hi " )
              (apply #'add-new-row current-lister new-values)
              (display-lister-view current-lister))
          (error (c)
            (logm:logm (format nil "Error creating new row: ~A" c))
            (message "Failed to create new row"))))
        (message "No active lister found"))))

;; Add key binding
(define-key lem/multi-column-list::*multi-column-list-mode-keymap* "C-c n" 'lister-menu/new-row)

;; Helper to get current row from the mcl
(defun get-current-row ()
  (let* ((window (current-window))
         (mcl (lem/multi-column-list:multi-column-list-of-window window)))
    (when (typep mcl 'lister-menu)
      (let ((focus-item (lem/multi-column-list::current-focus-item mcl)))
        (when focus-item
          (typecase focus-item
            (lem/multi-column-list::default-multi-column-list-item
                (lem/multi-column-list::default-multi-column-list-item-value focus-item))
            (t focus-item)))))))

;; Command for removing the current row
(define-command lister-menu/remove-row () ()
  (let ((current-lister (get-current-lister)))
    (if current-lister
        (handler-case
            (let ((current-row (get-current-row)))
              (if current-row
                  (progn
                    (remove-row current-lister current-row)
                    (quit-current-multi-column-list)
                    (display-lister-view current-lister))
                  (message "No row selected")))
          (error (c)
            (logm:logm (format nil "Error removing row: ~A" c))
            (message "Failed to remove row")))
        (message "No active lister found"))))

;; Add key binding for remove
(define-key lem/multi-column-list::*multi-column-list-mode-keymap* "C-c d" 'lister-menu/remove-row)

;
;; ROW MCL
(defclass row-values-lister (lister)
  ((source-row
    :initarg :source-row
    :accessor row-values-source-row)))

(defclass column-column (string-column) ()
  (:default-initargs
   :name "Column"))

(defclass value-column (string-column) ()
  (:default-initargs
   :name "Value"))

(defun create-row-values-lister (row)
  (let ((lister (make-instance 'row-values-lister
                               :name "row-values"
                               :source-row row
                               :columns (list
                                         (make-instance 'column-column)
                                         (make-instance 'value-column)))))
    ;; Convert row alist to the format needed for display
    (let ((values-as-rows
            (loop for (column . value) in row
                  collect (list (cons "Column" column)
                                (cons "Value" (princ-to-string value))))))
      (write-rows-to-file lister values-as-rows))
    lister))

(defun quit-current-multi-column-list ()
  (lem/multi-column-list:quit (lem/multi-column-list:multi-column-list-of-window (current-window))))


(define-command lister-menu/access-row () ()
  (let ((current-lister (get-current-lister)))
    (if current-lister
        (handler-case
            (let ((current-row (get-current-row)))
              (if current-row
                  (let ((values-lister (create-row-values-lister current-row)))
                    (quit-current-multi-column-list)
                    (display-lister-view values-lister))
                  (message "No row selected")))
          (error (c)
            (logm:logm (format nil "Error accessing row: ~A" c))
            (message "Failed to access row")))
        (message "No active lister found"))))

(define-key lem/multi-column-list::*multi-column-list-mode-keymap* "C-c a" 'lister-menu/access-row)
