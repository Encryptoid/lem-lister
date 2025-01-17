(in-package #:lem-lister)

;; COLUMN TEMPLATES
;; String Column
(defclass string-column (list-column) ()
  (:default-initargs
   ;; :handler #'handle-string-column
   :formatter #'column-formatter
   :new #'string-column-new))

(defun string-column-new (col)
  (prompt-for-string
   (format nil "Enter ~a: " (column-name col))
   ))

(defun handle-string-column (col)
  ;; To Clip
  )
;

;; File Column
(defclass file-column (list-column) ()
  (:default-initargs
   :formatter #'format-file-column
   :new #'file-column-new
   ))

(defun file-column-new (col)
  (prompt-for-file
   (format nil "Enter ~a: " col)
   :directory (current-directory)
   )
)

(defun format-file-column (col)
  col)

(defun handle-file-column (col)
  ;; To Clip
  )
;

;; Dir Column
(defclass dir-column (list-column) ()
  (:default-initargs
   ;; :handler #'handle-dir-column
   :formatter #'format-dir-column
   :new #'dir-column-new
   ))

(defun dir-column-new (col)
  (prompt-for-directory ""
                        :directory (buffer-directory)))

(defun format-dir-column (col)
  col)

(defun handle-dir-column (col)
  ;; To Clip
  )

;; Link Column
(defclass link-column (list-column) ()
  (:default-initargs
   :handler #'handle-link-column
   :formatter #'column-formatter
   :new #'string-column-new))

(defun handle-link-column (col)
  (logm:logm (format nil "Selected link: ~A" col))
  ;; (open-external-file ())
)
;


;; LISTER TEMPLATES
(defun path-lister-open (row)
  (logm:logm (format nil "open-file-lister called with row: ~A" row))
  (handler-case
      (let ((path (cdr (assoc "Path" row :test #'string=))))
        (lem:switch-to-buffer
         (lem/buffer/file::find-file-buffer path)
         )
        (logm:logm (format nil "Selected file: ~A" path)))
    (error (c)
      (logm:logm (format nil "Error in open-file-lister: ~A" c)))))

;; file-lister
(define-template file-lister
  (:default-initargs
   :name "file-lister" ;; TODO Don't default name
   :columns (list
             (make-instance 'string-column :name "Name")
             (make-instance 'file-column :name "File" :display-p t))
   :handler #'path-lister-open))
;

;; dir-lister
(define-template dir-lister
  (:default-initargs
   :name "dir-lister"
   :columns (list
             (make-instance 'string-column :name "Name")
             (make-instance 'dir-column :name "Path" :display-p t))
   :handler #'path-lister-open))
;

;; isearch-lister
(define-template isearch-lister
  (:default-initargs
   :name "isearch-lister"
   :columns (list
             (make-instance 'string-column :name "Name")
             (make-instance 'string-column :name "Search")
             (make-instance 'link-column :name "Linky"))
  :handler #'isearch-for-value))

(defun isearch-for-value (row)
  (lem/isearch::isearch-start (format nil "~A: " (cdr (assoc "Name" row :test #'string=)))
                              (lem/isearch::make-add-char-callback #'search-forward-regexp)
                              #'lem/isearch::search-forward-regexp
                              #'lem/isearch::search-backward-regexp
                              (cdr (assoc "Search" row :test #'string=))))

;; (define-template command-lister
;;   (:default-initargs
;;    :name "command-lister"
;;    :columns (list
;;              (make-instance 'string-column :name "Name")
;;              (make-instance 'dir-column :name "Path" :display-p t))
;;    :handler #'open-dir-lister))
