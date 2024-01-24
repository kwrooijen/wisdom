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

(defvar scripture-package-parameter-rules '(:straight replace :after replace))

(defun scripture-file-properties (file)
  "Return all properties from an org FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (let (properties)
      (goto-char (point-min))
      (while (re-search-forward
"^\\(?:;;[ \t]*\\)?#\\+\\(\\w+\\):[ \t]*\\(.*\\)$"
nil t)
        (let ((key (intern (downcase (match-string 1))))
              (value (match-string 2)))
          (push (cons key value) properties)))
      properties)))

(defun scripture-file-priority (file)
  "Return the priority of an org FILE.
If no priority is set, return 10."
  (let ((priority (alist-get 'priority (scripture-file-properties file) "10")))
    (if (string-match-p "^[0-9]+$" priority)
        (string-to-number priority)
      10)))

(defun scripture-get-use-package-package (params)
  "Return the package name and parameter of a `use-package' call.
Specified in the org babel header arguments PARAMS."
  (when-let ((filtered
              (seq-filter
               (lambda (x)
                 (string-match-p scripture-use-package-regex x))
               params)))
    (split-string (car filtered) "/")))

(defun scripture-wrap-in-conditional (file part)
  "Wrap PART in a condition-case form.
FILE is the file name of the Org file."
  (let ((body (plist-get part :body))
        (line (plist-get part :line)))
    (prin1-to-string
     `(condition-case err
          ,(read body)
        (error
         (progn
           (display-warning
            'scripture
            (format "Error loading %s:%s - %s"
                    ,(format "%s" file)
                    ,line
                    (error-message-string err)))
           (beep)))))))

(defun scripture-build-package (file package-name)
  "Build a `use-package' call for PACKAGE-NAME in string format."
  (when-let ((package (plist-get scripture-packages package-name)))
    (concat (format "(use-package %s\n" (symbol-name package-name))
            (let ((straight (or (plist-get (plist-get package :straight) :body) "t")))
              (format ":straight %s\n" straight))
            (when-let ((after (plist-get (plist-get package :after) :body)))
              (format ":after %s\n" after))
            (when-let ((init (plist-get package :init)))
              (format ":init %s\n" (string-join (mapcar (lambda (x) (scripture-wrap-in-conditional file x)) init))))
            (when-let ((config (plist-get package :config)))
              (format ":config %s\n" (string-join (mapcar (lambda (x) (scripture-wrap-in-conditional file x)) config))))
            ")")))

(defun scripture-plist-keys (plist)
  "Return the keys of PLIST as a list."
  (let ((keys '()))
    (while plist
      (push (car plist) keys)
      (setq plist (cddr plist)))
    (nreverse keys)))

(defun scripture-build-packages (file)
  "Build a string of `use-package' statements.
The resulting contains all all packages in `scripture-packages'."
  (let ((package-names (scripture-plist-keys scripture-packages))
        (result ""))
    (dolist (package-name package-names)
      (setq result (concat result (scripture-build-package file package-name))))
    result))

(defun scripture-add-package (package body element)
  "Execute a block of Use-Package code with org-babel.
PACKAGE is a list of the package name and parameter.
BODY is the body of the source block."
  (let* ((begin (org-element-property :begin (org-element-context)))
         (line (line-number-at-pos begin))
         (package-name (intern (car package)))
         (package-parameter (intern (concat ":" (car (cdr package)))))
         (previous-body (plist-get (plist-get scripture-packages package-name) package-parameter))
         (parameter-rule (plist-get scripture-package-parameter-rules package-parameter))
         (new-package-parameter
          (cond
           ((equal 'replace parameter-rule)
            (plist-put (plist-get scripture-packages package-name)
                       package-parameter
                       `(:body ,body :line ,line)))
           ((equal 'merge parameter-rule)
            ;; TODO
            (plist-put (plist-get scripture-packages package-name)
                       package-parameter
                       (append previous-body
                               `((:body ,body :line ,line)))))
           (t
            (plist-put (plist-get scripture-packages package-name)
                       package-parameter
                       (append previous-body
                               `((:body ,body :line ,line))))))))
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
                (scripture-add-package package body (org-element-context))
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
             (output (concat source "\n" (scripture-build-packages file))))
        (with-temp-file output-file
          (dolist (property (scripture-file-properties file))
            (insert (format ";; #+%s: %s\n\n"
                            (upcase (symbol-name (car property)))
                            (cdr property))))
          (insert output)))
      (when (file-exists-p output-file)
        (set-file-times output-file)))))

(defun scripture-get-files (extension directory)
  (let* ((files (directory-files-recursively directory extension)))
    (sort files (lambda (a b) (< (scripture-file-priority a)
                                 (scripture-file-priority b))))))

(defun scripture-compile-directory (&optional directory)
  "Compile all Org files in DIRECTORY to Elisp.
If DIRECTORY is nil, use `scripture-org-directory'."
  (dolist (file (scripture-get-files "\\.org$" (or scripture-org-directory directory)))
    (scripture-compile-file file)))

(defun scripture-parse-file (file)
  "Parse FILE and return a list of its top-level forms along with line numbers."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((forms '())
          (line-number 1))
      (while (not (eobp))
        (let ((form (ignore-errors (read (current-buffer)))))
          (when form
            (push (cons line-number form) forms)))
        (setq line-number (line-number-at-pos (point))))
      (nreverse forms))))

(defun scripture-load-directory (&optional directory)
  "Load all Elisp files in DIRECTORY.
If DIRECTORY is nil, use `scripture-output-directory'."
  (dolist (file (scripture-get-files "\\.el$" (or scripture-output-directory directory)))
    (let ((parsed (scripture-parse-file file)))
      (dolist (form parsed)
        (condition-case err
            (progn
              (message "Scripture: Loading %s" file)
              (eval (cdr form)))
          (error
           (progn
             (display-warning 'scripture (format "Error loading file: [%s] %s - %s"
                                                 (car form)
                                                 file
                                                 (error-message-string err)) :error)
             (beep))))))))

(defmacro scripture-bootstrap (&rest opts)
  "Bootstrap straight.el and `use-package'.
This macro should be called at the beginning of the init file.
If COMPILE-AND-LOAD is non-nil, compile and load the Elisp files."
  `(let ((compile-and-load (plist-get (quote ,opts) :compile-and-load))
         (user-org-directory (plist-get (quote ,opts) :org-directory)))
     (when user-org-directory
       (setq scripture-org-directory (eval user-org-directory)))
     (setq straight-repository-branch "develop")
     (setq gc-cons-threshold (* 1024 1024 100))
     (defvar bootstrap-version)
     (setq package-enable-at-startup nil)
     (let ((bootstrap-file
            (expand-file-name
             "straight/repos/straight.el/bootstrap.el"
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
       (require 'bind-key)
       (when compile-and-load
         (scripture-compile-directory)
         (scripture-load-directory))
       (setq gc-cons-threshold 800000)
       :done)))

(provide 'scripture)

;;; scripture.el ends here
