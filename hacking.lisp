(in-package #:lem-lister)

(define-template task-lister
  (:default-initargs
   :name "isearch-lister"
   :columns (list
             (make-instance 'string-column :name "Name")
             (make-instance 'bool-column :name "Current") ;; TODO init false
             (make-instance 'string-column :name "#")
             
             (make-instance 'string-column :name "Project")
             (make-instance 'string-column :name "Status") ;; TODO init todo
             (make-instance 'link-column :name "Ticket") ;; TODO init todo
             )
   :handler #'isearch-for-value))

(open-external-file url)


(define-lister-command 'isearch-lister "iSearch" "C-c l i")
(define-lister-command 'dir-lister "Workspaces" "C-c l w")
(define-lister-command 'file-lister "LemFiles" "C-c l l")
(define-lister-command 'file-lister "ConfigFiles" "C-c l c")

(setq ws-lister (make-instance 'dir-lister :name "Workspaces"))
(display-lister-view (get-lister "Workspaces"))

(make-instance 'isearch-lister :name "Test")
(lem:define-command lister-search () ()
  (display-lister-view (get-lister "Search")))
(define-key *global-keymap* "C-c l s" 'lister-search)

(define-lister-command-key 'isearch-lister "i" "C-c l i")

;; Usage:
(define-lister-command-key 'isearch-lister "Test" "C-c l a")

(setq lem-file-lister (make-instance 'file-lister :name "Lem Files"))
(display-lister-view (get-lister "Lem Files"))

(get-lister "Workspaces")

(lister-filename ws-lister)

(add-new-row ws-lister "My Project" "/home/projects/my-project")
(add-new-row ws-lister "Test" "/home/projects/my-project")


(prompt-for-file "Load File: "
                 :directory (or (buffer-filename) (buffer-directory))
                 :default nil
                 :existing t)

(lem:switch-to-buffer (lem/buffer/file::find-file-buffer "/home/l/notes/"))

;; (lem-core/commands/file:find-file-executor "/home/l/notes/todo.md")

(display-lister-view (get-lister "Workspaces"))
(describe lem/buffer/file:*find-directory-function*)

(prompt-for-file
 (format nil "Enter ~a: " "")
 )

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (get-lister "Workspaces")
    (let ((ws-lister (make-instance 'dir-lister :name "Workspaces")))
      (register-lister ws-lister))))

(lem:define-command lister-workspaces () ()
  (display-lister-view (get-lister "Workspaces")))

(display-lister-view (get-lister "Workspaces"))

(make-instance 'dir-lister
               :name "test1"
               :handler (lambda (item)
                          (lem:message "Selected file: ~A"
                                       (cdr (assoc "Name" (lister-item-row-data item)
                                                   :test #'string=)))))

;; Add some test data
(add-new-row (get-lister "test1") "file1.txt" "/path/to/file1.txt")

;; Display it
(display-lister-view (get-lister "test1"))





























