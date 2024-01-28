;;; scripture.el --- Managed Emacs configuration through org files

;;; Commentary:
;;

(require 'cl-lib)
(require 'ob-core)

;;; Code:

(defvar scripture-packages '()
  "List of packages that should be installed.")

(defcustom scripture-wrap-statements-in-condition t "Wrap code in condition statements."
  :type 'boolean
  :group 'scripture)

(defcustom scripture-use-package-keywords
  '("after" "straight" "config" "init" "bind" "bind_" "hook" "general" "custom")
  "List of `use-package' keywords that can be used.
The keywords are case sensitive. If the tag ends with an
underscore, it will be replaced with a asterisk."
  :type 'list
  :group 'scripture)

(defcustom scripture-output-directory "~/.emacs.d/scripture"
  "Directory where the tangled Elisp files are stored."
  :type 'string
  :group 'scripture)

(defcustom scripture-org-directory "~/.emacs.d/org"
  "Directory where the Org files are stored."
  :type 'string
  :group 'scripture)

(defun scripture-find-property (property)
  "Find PROPERTY in the current Org element or any ancestor element."
  (save-excursion
    (condition-case nil
        (progn
          (while (not (org-element-property property (org-element-context)))
            (org-up-element))
          (intern (org-element-property property (org-element-context))))
      (error nil))))

(defun scripture-find-tags ()
  "Find tags in the current Org element or any ancestor element."
  (save-excursion
    (condition-case error
        (progn
          (while (not (org-element-property :tags (org-element-lineage (org-element-context) '(headline) t)))
            (org-up-element))
          (org-element-property :tags (org-element-lineage (org-element-context) '(headline) t)))
      (error nil))))

(defun scripture-find-tag ()
  "Find a `use-package' tag in the current Org element or any ancestor element."
  (let* ((keywords scripture-use-package-keywords)
         (tag (car (seq-filter (lambda (tag) (member tag keywords)) (scripture-find-tags)))))
    (when tag
      (replace-regexp-in-string "_" "-"
                                (replace-regexp-in-string "_$" "*" tag)))))

(defun scripture-find-package ()
  "Find a `use-package' package in the current Org element or any ancestor element."
  (scripture-find-property :PACKAGE))

(defun scripture-find-after ()
  "Find a `use-package' after in the current Org element or any ancestor element."
  ;; Properties are symbols. Meaning (evil) is also a
  ;; symbol. Therefore we need to convert it to a string and read it.
  (when-let ((after (scripture-find-property :AFTER)))
    (prin1-to-string (read (symbol-name after)))))

(defun scripture-find-straight ()
  "Find a `use-package' straight in the current Org element or any
ancestor element."
  ;; Properties are symbols. Meaning (evil) is also a
  ;; symbol. Therefore we need to convert it to a string and read it.
  (when-let ((straight (scripture-find-property :STRAIGHT)))
    (prin1-to-string (read (symbol-name straight)))))

(defun scripture-find-requires ()
  "Find a `use-package' straight in the current Org element or any ancestor element."
  ;; Properties are symbols. Meaning (evil) is also a
  ;; symbol. Therefore we need to convert it to a string and read it.
  (when-let ((requires (scripture-find-property :REQUIRES)))
    (prin1-to-string (read (symbol-name requires)))))

(defun scripture-find-keyword ()
  "Find a `use-package' keyword in the current Org element or any ancestor element."
  (when-let ((keyword (scripture-find-property :KEYWORD)))
    (replace-regexp-in-string "^:" "" (symbol-name keyword))))

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

(defun scripture-get-use-package-package ()
  "Return the package name and parameter of a `use-package' call.
Specified in the org babel header arguments PARAMS."
  (when-let ((package (scripture-find-package))
             (keyword (or (scripture-find-keyword)
                          (scripture-find-tag))))
    (list package keyword)))

(defun scripture-safe-read (string file &optional line)
  "Read STRING and return the result.
If STRING is not a valid Elisp form, return nil.
FILE is the file name of the Org file.
LINE is the line number of the Org file.
If read fails, display a warning."
  (condition-case err
      (read string)
    (error
     (progn
       (display-warning
        'scripture
        (if line
            (format "failed to read body of %s:%s" file line)
          (format "failed to read body of %s" file))
        :error)
      nil ))))

(defun scripture-wrap-in-condition (file part)
  "Wrap PART in a `condition-case' form.
FILE is the file name of the Org file."
  (let* ((body (plist-get part :body))
         (line (plist-get part :line))
         (expression (scripture-safe-read (format "(progn %s)" body) file line)))
    (pp-to-string
     (if scripture-wrap-statements-in-condition
         `(condition-case err
              ,expression
            (error
             (progn
               (display-warning
                'scripture
                (format "Error loading %s:%s - %s"
                        ,(format "%s" file)
                        ,line
                        (error-message-string err))
                :error))))
       expression))))

(defun scripture-merge-bodies (file xs)
  "Merge the bodies of a list of `use-package' statements.
FILE is the file name of the Org file.
XS is a list of `use-package' statements."
  (let ((result '()))
    (dolist (x xs)
      (let* ((body (plist-get x :body))
             (line (plist-get x :line))
             (result-body (scripture-safe-read body file line)))
        (when result-body
          (setq result (append result result-body)))))
    (when result
      (prin1-to-string result))))

(defun scripture-build-package-string (package-name package file)
  "Build a `use-package' call for PACKAGE-NAME in string format.
PACKAGE is the package plist.
FILE is the file name of the Org file."
  (concat (format "(use-package %s\n" package-name)
          (when-let ((straight (plist-get (car (plist-get package :straight)) :body)))
            (format ":straight %s\n" straight))
          (when-let ((requires (plist-get (car (plist-get package :requires)) :body)))
            (format ":requires %s\n" requires))
          (when-let ((after (plist-get (car (plist-get package :after)) :body)))
            (format ":after %s\n" after))
          (when-let ((bind* (scripture-merge-bodies file (plist-get package :bind*))))
            (format ":bind* %s\n" bind*))
          (when-let ((bind (scripture-merge-bodies file (plist-get package :bind))))
            (format ":bind %s\n" bind))
          (when-let ((hook (scripture-merge-bodies file (plist-get package :hook))))
            (format ":hook %s\n" hook))
          (when-let ((init (plist-get package :init)))
            (format ":init %s\n" (string-join (mapcar (lambda (x) (scripture-wrap-in-condition file x)) init))))
          (when-let ((config (plist-get package :config)))
            (format ":config %s\n" (string-join (mapcar (lambda (x) (scripture-wrap-in-condition file x)) config))))
          (when-let ((general (plist-get package :general)))
            (format ":general %s\n" (string-join (mapcar (lambda (part) (plist-get part :body)) general))))
          (when-let ((custom (plist-get package :custom)))
            (format ":custom %s\n" (string-join (mapcar (lambda (part) (plist-get part :body)) custom))))

          ")"))

(defun scripture-build-package (file package-name)
  "Build a `use-package' call for PACKAGE-NAME in string format.
FILE is the file name of the Org file.
PACKAGE-NAME is the name of the package."
  (when-let ((package (plist-get scripture-packages package-name)))
    (when (not (equal package-name (intern "nil")))
      (let ((package-string (scripture-build-package-string package-name package file)))
        (pp-to-string
         (scripture-safe-read package-string file))))))

(defun scripture-plist-keys (plist)
  "Return the keys of PLIST as a list."
  (let ((keys '()))
    (while plist
      (push (car plist) keys)
      (setq plist (cddr plist)))
    (nreverse keys)))

(defun scripture-build-packages (file)
  "Build a string of `use-package' statements.
The resulting contains all all packages in `scripture-packages'.
FILE is the file name of the Org file."
  (let ((package-names (scripture-plist-keys scripture-packages))
        (result ""))
    (dolist (package-name package-names)
      (setq result (concat result (scripture-build-package file package-name))))
    result))

(defun put-package-parameter (package-name parameter value)
  "Put a parameter in the `scripture-packages' plist.
PACKAGE-NAME is the name of the package.
PARAMETER is the parameter to set.
VALUE is the value to set."
  (setq scripture-packages
        (plist-put
         scripture-packages
         package-name
         (plist-put (plist-get scripture-packages package-name)
                    parameter
                    value))))

(defun scripture-add-package (package body element)
  "Execute a block of Use-Package code with org-babel.
PACKAGE is a list of the package name and parameter.
BODY is the body of the source block.
ELEMENT is the org element of the source block."
  (let* ((begin (org-element-property :begin element))
         (line (line-number-at-pos begin))
         (package-name (car package))
         (package-parameter (intern (concat ":" (car (cdr package)))))
         (previous-body (plist-get (plist-get scripture-packages package-name) package-parameter))
         (value (append previous-body `((:body ,body :line ,line)))))
    (put-package-parameter package-name package-parameter value)
    nil))

(defun scripture-execute-org-src-blocks-and-capture-results (file)
  "Execute all source blocks in FILE and return the results as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (let ((results '()))
      (org-map-entries
       (lambda ()
         (let ((line (line-number-at-pos (org-element-property :begin (org-element-context)))))
           (when-let ((package-name (scripture-find-package))
                      (package (org-element-property :PACKAGE (org-element-context))))
             (put-package-parameter package-name :package line))
           (when-let ((package-name (scripture-find-package))
                      (after (scripture-find-after)))
             (put-package-parameter package-name :after `((:body ,after :line ,line))))
           (when-let ((package-name (scripture-find-package))
                      (straight (scripture-find-straight)))
             (put-package-parameter package-name :straight `((:body ,straight :line ,line))))
           (when-let ((package-name (scripture-find-package))
                      (requires (scripture-find-requires)))
             (put-package-parameter package-name :requires `((:body ,requires :line ,line)))))))
      (org-babel-map-src-blocks nil
        (let ((body (org-element-property :value (org-element-context)))
              (line (line-number-at-pos (org-element-property :begin (org-element-context))))
              (language (org-element-property :language (org-element-context))))
          (when (string= language "emacs-lisp")
            (if-let ((package (scripture-get-use-package-package)))
                (scripture-add-package package body (org-element-context))
              (when (stringp body)
                (push (scripture-wrap-in-condition file `(:body ,body :line ,line))
                      results))))))
      (mapconcat 'identity (reverse results) "\n"))))

(defun scripture-output-file-name (file)
  "Return the name of the output Elisp file for FILE."
  (if (string-prefix-p scripture-org-directory file)
      (concat (file-name-as-directory scripture-output-directory)
              (file-name-sans-extension (substring file (length scripture-org-directory)))
              ".el")
    (error "File is not in scripture-org-directory")))

(defun scripture-compile-file (file)
  "Compile FILE to Elisp.
FILE is an Org file.
The output Elisp file is stored in `scripture-output-directory'."
  (unless (file-exists-p file)
    (error "File to tangle does not exist: %s" file))
  (unless (file-exists-p scripture-output-directory)
    (make-directory scripture-output-directory))
  (let ((output-file (scripture-output-file-name file)))
    (make-directory (file-name-directory output-file) t)
    (when (file-newer-than-file-p file output-file)
      (message "Scripture: Compiling %s" file)
      (let* ((scripture-packages nil)
             (source  (scripture-execute-org-src-blocks-and-capture-results file))
             (output (concat source "\n" (scripture-build-packages file))))
        (with-temp-file output-file
          (dolist (property (scripture-file-properties file))
            (insert (format ";; #+%s: %s\n\n"
                            (upcase (symbol-name (car property)))
                            (cdr property))))
          (insert output)))
      (when (file-exists-p output-file)
        (set-file-times output-file))
      output-file)))

(defun scripture-get-files (extension directory)
  "Return all files with EXTENSION in DIRECTORY.
The files are sorted by priority."
  (let* ((files (directory-files-recursively directory extension)))
    (sort files (lambda (a b) (< (scripture-file-priority a)
                                 (scripture-file-priority b))))))

(defun scripture-compile-directory ()
  "Compile all Org files in `scripture-org-directory' to Elisp.
All files will be outputted to `scripture-output-directory'."
  (let ((compiled '()))
    (dolist (file (scripture-get-files "^[^#]*\\.org$" scripture-org-directory))
      (when-let ((output-file (scripture-compile-file file)))
        (push output-file compiled)))
    compiled))

(defun scripture-load-file (file)
  "Load FILE."
  (let ((inhibit-message t))
    (load file nil t)))

(defun scripture-load-directory ()
  "Load all Elisp files in `scripture-output-directory'. "
  (let ((initial-gc-cons-threshold gc-cons-threshold))
    (setq gc-cons-threshold (* 1024 1024 100))
    (dolist (file (scripture-get-files "^[^#]*\\.el$" scripture-output-directory))
      (scripture-load-file file))
    (setq gc-cons-threshold initial-gc-cons-threshold)))

(defun scripture-reload ()
  "Compile and load all Org files."
  (interactive)
  (dolist (compiled-file (scripture-compile-directory))
    (scripture-load-file compiled-file)))

(defun scripture-preview ()
  "Compile the current buffer and display the result in *scripture preview*."
  (interactive)
  (let* ((buffer (get-buffer-create "*scripture preview*"))
         ;; We display the buffer first, then compile. If there is an
         ;; error it will not be overruled by the preview buffer.
         (_ (display-buffer buffer))
         (scripture-wrap-statements-in-condition nil)
         (file (buffer-file-name (current-buffer)))
         (scripture-packages nil)
         (source (scripture-execute-org-src-blocks-and-capture-results file))
         (output (concat source "\n" (scripture-build-packages file))))
    (with-current-buffer buffer
      (emacs-lisp-mode)
      (read-only-mode 1)
      (save-excursion
        (let ((inhibit-read-only t))
          (replace-region-contents (point-min) (point-max) (lambda () output)))))))

(define-minor-mode scripture-preview-mode
  "Preview the current buffer as elisp."
  :lighter " scripture-preview"
  (if scripture-preview-mode
      (add-hook 'after-save-hook 'scripture-preview nil t)
    (remove-hook 'after-save-hook 'scripture-preview t)))

(provide 'scripture)

;; TODO Add "push" to loading blocks / files so have an indicator that they're loaded.

;;; scripture.el ends here
