(in-package #:lem-lister)

(setq ws-lister (make-instance 'dir-lister :name "Workspaces"))
(register-lister ws-lister)

(add-new-row ws-lister "My Project" "/home/projects/my-project")
(add-new-row ws-lister "Test" "/home/projects/my-project")


(display-lister-view (get-lister "Workspaces"))

;; (lem:define-command lister-workspaces () ()
;;   (display-lister-view (get-lister "Workspaces")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (lem:define-command lister-workspaces () ()
    (ensure-view-exists (lister-class-to-view-name
                          (type-of (get-lister "Workspaces"))))
    (display-lister-view (get-lister "Workspaces"))))
