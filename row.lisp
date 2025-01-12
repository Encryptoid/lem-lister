(in-package #:lem-lister)

(defun set-row-values (row lister values)
  "Set values for a row based on lister's columns"
  (let ((columns (lister-columns lister)))
    (when (/= (length columns) (length values))
      (error "Number of values (~A) doesn't match number of columns (~A)"
             (length values) (length columns)))

    ;; Create association list of column-name -> value pairs
    (setf (row-values row)
          (loop for column in columns
                for value in values
                collect (cons (field-name column) value)))

    ;; Get current rows and update file
    (let ((current-rows (read-rows-from-file lister)))
      (write-rows-to-file lister (cons row current-rows)))
    row))

;; (defun make-lister-row (column-values)
;;   "Create a row from a list of values in column order"
;;   (make-instance 'lister-row))

(defun make-row-alist (lister values)
    "Create an alist mapping column names to values"
    (let ((columns (lister-columns lister)))
      (when (/= (length columns) (length values))
        (error "Number of values (~A) doesn't match number of columns (~A)"
               (length values) (length columns)))
      (loop for column in columns
            for value in values
            collect (cons (field-name column) value))))

(defun add-new-row (lister &rest values)
    "Add a new row to a lister with the given values"
    (let* ((row-alist (make-row-alist lister values))
           (current-rows (read-rows-from-file lister)))
      (write-rows-to-file lister (cons row-alist current-rows))
      row-alist))

(defun write-rows-to-file (lister rows)
  "Write all rows to the lister's file"
  (with-open-file (stream (lister-filename lister)
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (prin1 rows stream)))

(defun read-rows-from-file (lister)
  "Read all rows from the lister's file"
  (with-open-file (stream (lister-filename lister)
                          :direction :input
                          :if-does-not-exist :create)
    (or (ignore-errors (read stream)) nil)))
