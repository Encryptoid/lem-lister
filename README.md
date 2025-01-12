Readme todos:
 - TODO - Expose *lister-dir* and explain
 - TODO - Expose keybindings and explain 
 - TODO - Remove unneeded commands
 - TODO - Explain display-p

# lem-lister

## Current State
This is still only just being put together. Everything is open to change still, but the code base isn't that big, there isn't that much to have to change in your config if I change the app.

Expect bugs for sure, and please let me know if you encounter any, either on Discord or via an Issue on Github.
If you wish to request new functionality, or discuss if something would fit in the project, then you can do the same.


## TODO
 - Allow another lister to be a column, where new will give you a choice of that lists values.
 - Allow dynamic columns that are not stored and are evaluated when the list is opened
 - More columns:
    - link
    - bool
 - Add keybinds to promote and demote items in the list.

## Introduction
Lem Lister is a package that leverages Lem's multi-column-list functionality to allow you to create your own lists.
These lists can be created from templates allowing distinct functionality. With each template, you can create many lists.
The data will be stored as a list in file so will persist between sessions.


## Installation
Download this repo to a directory and load it in your user config.

```common-lisp
(asdf:load-asd (probe-file "<path-to-lem-lister-dir>/lem-lister.asd"))
(ql:quickload :lem-lister)
```


## Setup
After installation, you can define lists in your config with the macro: `define-lister-command`, which will create for you: a lister, a command, and optionally a keybinding:
```common-lisp
(define-lister-command 'isearch-lister "Search" "C-c l i") ;; Produces command `lister-search`
(define-lister-command 'dir-lister "Workspaces") ;; Produces command `lister-workspaces`
```

When you create your list it will be empty and not display anything. `C-c n` will initiate a new row based on your columns.

`C-c d` to remove

## Templates
Lister lists are built off templates. These templates define functionality about the list. For now each template can define the list's:
  - Columns
  - Action Handler

### Basic Example
Looking at the core lister template `file-lister`, we can see it's definition here:
```common-lisp
(define-template file-lister
  (:default-initargs
   :name "file-lister"
   :columns (list ;; Column Definitions
             (make-instance 'string-column :name "Name")
             (make-instance 'file-column :name "File" :display-p t))
   :handler #'path-lister-open)) ;; Handler function will assoc `File` from selected row and open it as a path(shared with dir-lister)
```

We can see that it has 2 columns, `Name` & `File`. `Name` is a `string-column` that isn't strictly necessary for files, but you can give a nicer name to search.
`File` is a `file-column`.

When we request to create a new lister entry, lister will loop through each column, and depending on the type of column, will perform the appropriate data acquisition.
This means that for a `string-column` it will `prompt-for-string`, and `file-column` will `prompt-for-file`.

```common-lisp
(defclass file-column (list-column) ()
  (:default-initargs
   :formatter #'format-file-column
   :new #'file-column-new
   ))
```

There are core templates provided by the package, but you are able to create your own in the same way.


### Core
Currently the list of core columns are:
  - file-column
  - dir-column
  - isearch-column

The current list of core listers are below. Feel free to add contribute more if they would be useful. I have not yet added some simple ones like for grep'ing, etc.

#### file-lister
Description: Allows storing a list of files.
Columns: Name | File
Use Case: You can have a system wide one, or area specific ones such as one for "LemFiles" which saves specific Lem files you are interested in.
```common-lisp
(define-lister-command-key 'file-lister "LemFiles" "C-c l l")
(define-lister-command-key 'file-lister "ConfigFiles" "C-c l c")
```


#### dir-lister
Columns: Name | Dir
Description: Allows storing a list of directories.
Use Case: Useful for storing a list of project to get around to.


#### isearch-lister
Columns: Name | Search
Description: Allows storing a list of regexes to seach.
Use Case: Save a list of regexes, and the action handler will perform them on your current buffer.
You might have an `isearch-lister` for Lem searches such as `define-command`, `define-key`


### Custom Templates
You can define your own custom templates.

#### Custom Columns
If you need column functionality beyond the core, you will need to define a column, and it's appropriate `:new` & `:formatter` action.
Keep in mind that these do not need to request input from the user, and can simply run code(eg. get data out of the current dir and store it in the column.)
```common-lisp
(in-package #:lem-lister)

(defclass string-column (list-column) ()
  (:default-initargs
   :formatter #'column-formatter
   :new #'string-column-new))

(defun string-column-new (col)
  (prompt-for-string
   (format nil "Enter ~a: " (column-name col))
   ))
```

Then you can define your template similar to this:
```common-lisp
(define-template file-lister
  (:default-initargs
   :name "file-lister"
   :columns (list ;; Column Definitions
             (make-instance 'string-column :name "Name")
             (make-instance 'file-column :name "File" :display-p t))
   :handler #'path-lister-open)) ;; Handler function will assoc `File` from selected row and open it as a path(shared with dir-lister)
```
