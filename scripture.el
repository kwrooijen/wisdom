;;; scripture.el --- Managed Emacs configuration through org files

;;; Commentary:
;;

(require 'cl-lib)

;;; Code:

(defvar scripture-packages '()
  "List of packages that should be installed.")

(defcustom scripture-use-package-regex "[a-z\\-]+/\\(after\\|straight\\|config\\|init\\)"
  "Regular expression to match against `use-package' parameters."
  :type 'string
  :group 'scripture)

(defvar scripture-package-parameter-rules '(:straight replace :after merge))

(defun scripture-get-use-package-package (params)
  "Return the package name and parameter of a `use-package' call.
Specified in the org babel header arguments PARAMS."
  (when-let ((filtered
              (seq-filter
               (lambda (x)
                 (string-match-p scripture-use-package-regex x))
               params)))
    (split-string (car filtered) "/")))

(defun scripture-build-package (package-name)
  "Build a `use-package' call for PACKAGE-NAME in string format."
  (when-let ((package (plist-get scripture-packages package-name)))
    (concat (format "(use-package %s\n" (symbol-name package-name))
            (let ((straight (or (plist-get package :straight) "t")))
	      (format ":straight %s\n" straight))
            (when-let ((after (plist-get package :after)))
              (format ":after %s\n" after))
            (when-let ((init (plist-get package :init)))
              (format ":init %s\n" (string-join init)))
            (when-let ((config (plist-get package :config)))
              (format ":config %s\n" (string-join config)))
            ")")))

(defun scripture-plist-keys (plist)
  "Return the keys of PLIST as a list."
  (let ((keys '()))
    (while plist
      (push (car plist) keys)
      (setq plist (cddr plist)))
    (nreverse keys)))

(defun scripture-build-packages ()
  "Build a string of `use-package' statements.
The resulting contains all all packages in `scripture-packages'."
  (let ((package-names (scripture-plist-keys scripture-packages))
        (result ""))
    (dolist (package-name package-names)
      (setq result (concat result (scripture-build-package package-name))))
    result))

(defun scripture-add-package (package body)
  "Execute a block of Use-Package code with org-babel.
PACKAGE is a list of the package name and parameter.
BODY is the body of the source block."
  (let* ((package-name (intern (car package)))
         (package-parameter (intern (concat ":" (car (cdr package)))))
         (previous-body (plist-get (plist-get scripture-packages package-name) package-parameter))
         (parameter-rule (plist-get scripture-package-parameter-rules package-parameter))
         (new-package-parameter
          (cond
           ((equal 'replace parameter-rule)
            (plist-put (plist-get scripture-packages package-name)
                       package-parameter body))

           ((equal 'merge parameter-rule)
            ;; TODO
            (plist-put (plist-get scripture-packages package-name)
                       package-parameter
                       (cl-remove-duplicates (append previous-body (list body))
                                             :test 'string-equal)))
           (t
            (plist-put (plist-get scripture-packages package-name)
                       package-parameter
                       (cl-remove-duplicates (append previous-body (list body))
                                             :test 'string-equal))))))
    (setq scripture-packages
          (plist-put scripture-packages package-name new-package-parameter))
    nil))

(defun scripture-execute-org-src-blocks-and-capture-results (file)
  "Execute all source blocks in FILE and return the results as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (let ((results '()))
      (org-babel-map-src-blocks nil
        (let ((body (org-element-property :value (org-element-context)))
              (language (org-element-property :language (org-element-context)))
              (parameters (split-string (or (org-element-property :parameters (org-element-context)) "") " ")))
          (when (string= language "emacs-lisp")
            (if-let ((package (scripture-get-use-package-package parameters)))
                (scripture-add-package package body)
              (when (stringp body)
                (push body results))))))
      (mapconcat 'identity (reverse results) "\n"))))

(defcustom scripture-output-directory "~/.emacs.d/scripture"
  "Directory where the tangled Elisp files are stored."
  :type 'string
  :group 'scripture)

(defcustom scripture-org-directory "~/.emacs.d/org"
  "Directory where the Org files are stored."
  :type 'string
  :group 'scripture)

(defun scripture-output-file-name (file)
  "Return the name of the output Elisp file for FILE."
  (concat (file-name-as-directory scripture-output-directory)
          (file-name-nondirectory (file-name-sans-extension file))
          ".el"))

(defun scripture-compile-file (file)
  "Compile FILE to Elisp.
FILE is an Org file.
The output Elisp file is stored in `scripture-output-directory'."
  (unless (file-exists-p file)
    (error "File to tangle does not exist: %s" file))
  (unless (file-exists-p scripture-output-directory)
    (make-directory scripture-output-directory))
  (let ((output-file (scripture-output-file-name file)))
    (when (file-newer-than-file-p file output-file)
      (let* ((scripture-packages nil)
             (source (scripture-execute-org-src-blocks-and-capture-results file))
             (output (concat source "\n" (scripture-build-packages))))
	(with-temp-file output-file
          (insert output)))
      (when (file-exists-p output-file)
        (set-file-times output-file))
      (byte-compile-file output-file))))

(defun scripture-compile-directory (&optional directory)
  "Compile all Org files in DIRECTORY to Elisp.
If DIRECTORY is nil, use `scripture-org-directory'."
  (let ((files (directory-files-recursively (or scripture-org-directory directory) "\\.org$")))
    (dolist (file files)
      (scripture-compile-file file))))

(defun scripture-load-directory (&optional directory)
  "Load all Elisp files in DIRECTORY.
If DIRECTORY is nil, use `scripture-output-directory'."
  (let ((files (directory-files-recursively (or scripture-output-directory directory) "\\.elc$")))
    (dolist (file files)
      (load-file file))))

(defmacro scripture-bootstrap (&optional compile-and-load)
  "Bootstrap straight.el and `use-package'.
This macro should be called at the beginning of the init file.
If COMPILE-AND-LOAD is non-nil, compile and load the Elisp files."
  `(progn
     (defvar bootstrap-version)
     (setq package-enable-at-startup nil)
     (let ((bootstrap-file
            (expand-file-name
             "~/.emacs.d/straight/repos/straight.el/bootstrap.el"
             (or (bound-and-true-p straight-base-dir)
                 user-emacs-directory)))
           (bootstrap-version 7))
       (unless (file-exists-p bootstrap-file)
         (with-current-buffer
             (url-retrieve-synchronously
              "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
              'silent 'inhibit-cookies)
           (goto-char (point-max))
           (eval-print-last-sexp)))
       (load bootstrap-file nil 'nomessage)
       (straight-use-package 'use-package)
       (straight-use-package 'org)
       (when ,compile-and-load
         (scripture-compile-directory)
         (scripture-load-directory)))))

(provide 'scripture)

;;; scripture.el ends here
