;;; wisdom.el --- Managed Emacs configuration through org files

;; Copyright (C) 2025  Kevin W. van Rooijen

;; Author: Kevin W. van Rooijen

;;; Commentary:
;;

(require 'cl-lib)
(require 'ob-core)
(require 'url)

;;; Code:

(defvar wisdom-packages '()
  "List of packages that should be installed.")

(defvar wisdom-compiling-remote nil
  "Whether we are compiling a remote file.")

(defvar wisdom--booting nil
  "Non-nil when running inside `wisdom-boot'.")

(defvar wisdom--boot-phase :loading
  "Internal boot phase for the splash screen.
Possible values: :compiling or :loading.")

(defvar wisdom--boot-errors '()
  "List of compile / loading errors encountered during wisdom boot.")

(defcustom wisdom-wrap-statements-in-condition t "Wrap code in condition statements."
  :type 'boolean
  :group 'wisdom)

(defcustom wisdom-use-package-keywords
  '("after" "demand" "straight" "config" "init" "bind" "bind_" "hook" "general" "custom")
  "List of `use-package' keywords that can be used.
The keywords are case sensitive. If the tag ends with an
underscore, it will be replaced with a asterisk."
  :type 'list
  :group 'wisdom)

(defcustom wisdom-output-directory "~/.emacs.d/wisdom"
  "Directory where the tangled Elisp files are stored."
  :type 'string
  :group 'wisdom)

(defcustom wisdom-org-directory "~/.emacs.d/org"
  "Directory where the Org files are stored."
  :type 'string
  :group 'wisdom)

(defcustom wisdom-remote-output-directory "~/.emacs.d/remote-wisdom"
  "Directory where the remote tangled Elisp files are stored."
  :type 'string
  :group 'wisdom)

(defcustom wisdom-remote-org-directory "~/.emacs.d/remote-org"
  "Directory where the remote Org files are stored."
  :type 'string
  :group 'wisdom)

(defcustom wisdom-force-compile nil
  "Force compilation of Org files, even if the Elisp file is newer
than the Org file."
  :type 'boolean
  :group 'wisdom)

(defcustom wisdom-force-download nil
  "Force download of remote Org files, even if the file already"
  :type 'boolean
  :group 'wisdom)


(defun wisdom-indent (string n)
  "Indent STRING by N spaces."
  (let ((indentation (make-string n ?\s)))
    (replace-regexp-in-string "^" indentation string)))

(defun wisdom-get-org-directory ()
  "Return the Org directory."
  (if wisdom-compiling-remote
      (expand-file-name wisdom-remote-org-directory)
    (expand-file-name wisdom-org-directory)))

(defun wisdom-get-output-directory ()
  "Return the output directory."
  (if wisdom-compiling-remote
      (expand-file-name wisdom-remote-output-directory)
    (expand-file-name wisdom-output-directory)))

(defun wisdom-find-property (property)
  "Find PROPERTY in the current Org element or any ancestor element."
  (save-excursion
    (condition-case nil
        (progn
          (while (not (org-element-property property (org-element-context)))
            (org-up-element))
          (intern (org-element-property property (org-element-context))))
      (error nil))))

(defun wisdom-find-tags ()
  "Find tags in the current Org element or any ancestor element."
  (save-excursion
    (condition-case error
        (progn
          (while (not (org-element-property :tags (org-element-lineage (org-element-context) '(headline) t)))
            (org-up-element))
          (org-element-property :tags (org-element-lineage (org-element-context) '(headline) t)))
      (error nil))))

(defun wisdom-find-tag ()
  "Find a `use-package' tag in the current Org element or any ancestor element."
  (let* ((keywords wisdom-use-package-keywords)
         (tag (car (seq-filter (lambda (tag) (member tag keywords)) (wisdom-find-tags)))))
    (when tag
      (replace-regexp-in-string "_" "-"
                                (replace-regexp-in-string "_$" "*" tag)))))

(defun wisdom-find-package ()
  "Find a `use-package' package in the current Org element or any ancestor element."
  (or (wisdom-find-property :PACKAGE)
      (wisdom-find-property :USE_PACKAGE)
      (wisdom-find-property :USE-PACKAGE)))

(defun wisdom-find-after ()
  "Find a `use-package' after in the current Org element or any ancestor element."
  ;; Properties are symbols. Meaning (evil) is also a
  ;; symbol. Therefore we need to convert it to a string and read it.
  (when-let* ((after (wisdom-find-property :AFTER)))
    (prin1-to-string (read (symbol-name after)))))

(defun wisdom-find-demand ()
  "Find a `use-package' demand in the current Org element or any ancestor element."
  ;; Properties are symbols. Meaning (evil) is also a
  ;; symbol. Therefore we need to convert it to a string and read it.
  (when-let* ((demand (wisdom-find-property :DEMAND)))
    (prin1-to-string (read (symbol-name demand)))))

(defun wisdom-find-straight ()
  "Find a `use-package' straight in the current Org element or any ancestor element."
  ;; Properties are symbols. Meaning (evil) is also a
  ;; symbol. Therefore we need to convert it to a string and read it.
  (when-let* ((straight (wisdom-find-property :STRAIGHT)))
    (prin1-to-string (read (symbol-name straight)))))

(defun wisdom-find-defer ()
  "Find a `use-package' defer in the current Org element or any ancestor element."
  ;; Properties are symbols. Meaning (evil) is also a
  ;; symbol. Therefore we need to convert it to a string and read it.
  (when-let* ((defer (wisdom-find-property :DEFER)))
    (prin1-to-string (read (symbol-name defer)))))

(defun wisdom-find-requires ()
  "Find a `use-package' straight in the current Org element or any ancestor element."
  ;; Properties are symbols. Meaning (evil) is also a
  ;; symbol. Therefore we need to convert it to a string and read it.
  (when-let* ((requires (wisdom-find-property :REQUIRES)))
    (prin1-to-string (read (symbol-name requires)))))

(defun wisdom-find-keyword ()
  "Find a `use-package' keyword in the current Org element or any ancestor element."
  (when-let* ((keyword (wisdom-find-property :KEYWORD)))
    (replace-regexp-in-string "^:" "" (symbol-name keyword))))

(defun wisdom-file-properties (file)
  "Return all properties from an org FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (let (properties)
      (goto-char (point-min))
      (while (re-search-forward "^\\(?:;;[ \t]*\\)?#\\+\\([A-Za-z0-9_]+\\):[ \t]*\\(.*\\)$" nil t)
        (let ((key (intern (downcase (match-string 1))))
              (value (match-string 2)))
          (push (cons key value) properties)))
      properties)))

(defun wisdom-file-priority (file)
  "Return the priority of an org FILE.
If no priority is set, return 10."
  (let ((priority (alist-get 'priority (wisdom-file-properties file) "10")))
    (if (string-match-p "^[0-9]+$" priority)
        (string-to-number priority)
      10)))

(defun wisdom-file-remote (file)
  "Return the remote property of an org FILE."
  (let ((remote (alist-get 'remote (wisdom-file-properties file))))
    ;; Read string as elisp list
    (when remote
      (read remote))))

(defun wisdom-file-lexical-binding (file)
  "Return the lexical-binding of an org FILE.
If no lexical-binding is set, return t."
  (let ((lexical-binding (alist-get 'lexical_binding (wisdom-file-properties file) "t")))
    (if (string= lexical-binding "nil")
        nil
      t)))

(defun wisdom-get-use-package-package ()
  "Return the package name and parameter of a `use-package' call.
Specified in the org babel header arguments PARAMS."
  (when-let* ((package (wisdom-find-package))
             (keyword (or (wisdom-find-keyword)
                          (wisdom-find-tag))))
    (list package keyword)))

(defun wisdom-safe-read (string file &optional line)
  "Read STRING and return the result.
If STRING is not a valid Elisp form, return nil.
FILE is the file name of the Org file.
LINE is the line number of the Org file.
If read fails, display a warning."
  (condition-case err
      ;; TODO The following expression does not fail. Even though there is a syntax error.
      ;; (read "(progn (1)) hello)") ;;=> (progn (1))
      (read string)
    (error
     (progn
       (display-warning
        'wisdom
        (if line
            (format "failed to read body of %s:%s" file line)
          (format "failed to read body of %s" file))
        :error)
       nil))))

(defun wisdom-wrap-in-condition (file part)
  "Wrap PART in a `condition-case' form.
FILE is the file name of the Org file."
  (let* ((body (plist-get part :body))
         (line (plist-get part :line))
         (expression-string (string-trim-right body))
         (expression (wisdom-safe-read (format "(progn\n%s)" expression-string) file line)))
    (if wisdom-wrap-statements-in-condition
        (pp-to-string
         (if wisdom-wrap-statements-in-condition
             `(condition-case err
                  ,expression
                (error
                 (add-to-list 'wisdom--boot-errors
                              (list :file ,(format "%s" file)
                                    :line ,line
                                    :message (error-message-string err)))
                 (unless wisdom--booting
                   (display-warning
                    'wisdom
                    (format "Error loading %s:%s - %s"
                            ,(format "%s" file)
                            ,line
                            (error-message-string err))
                    :error))))
           expression))
      expression-string)))

(defun wisdom-merge-bodies (file xs)
  "Merge the bodies of a list of `use-package' statements.
FILE is the file name of the Org file.
XS is a list of `use-package' statements."
  (let ((result '()))
    (dolist (x xs)
      (let* ((body (plist-get x :body))
             (line (plist-get x :line))
             (result-body (wisdom-safe-read body file line)))
        (when result-body
          (setq result (append result result-body)))))
    (when result
      (prin1-to-string result))))

(defun wisdom-build-package-string (package-name package file)
  "Build a `use-package' call for PACKAGE-NAME in string format.
PACKAGE is the package plist.
FILE is the file name of the Org file."
  ;; TODO wrap in condition-case
  (concat
   (string-trim-right
    ;; TODO use canonical order
    ;; :disabled / :if / :when
    ;; :preface
    ;; :after
    ;; :commands
    ;; :bind / :general
    ;; :hook
    ;; :init
    ;; :custom
    ;; :config
    (concat (format "(use-package %s" package-name)
            (when-let* ((straight (plist-get (car (plist-get package :straight)) :body)))
              (format "\n  :straight %s" straight))
            (when-let* ((defer (plist-get (car (plist-get package :defer)) :body)))
              (format "\n  :defer %s" defer))
            (when-let* ((requires (plist-get (car (plist-get package :requires)) :body)))
              (format "\n  :requires %s" requires))
            (when-let* ((after (plist-get (car (plist-get package :after)) :body)))
              (format "\n  :after %s" after))
            (when-let* ((demand (plist-get (car (plist-get package :demand)) :body)))
              (format "\n  :demand %s" demand))
            (when-let* ((bind* (wisdom-merge-bodies file (plist-get package :bind*))))
              (format "\n  :bind*\n%s" (wisdom-indent bind* 2)))
            (when-let* ((bind (wisdom-merge-bodies file (plist-get package :bind))))
              (format "\n  :bind\n%s" (wisdom-indent bind 2)))
            (when-let* ((hook (wisdom-merge-bodies file (plist-get package :hook))))
              (format "\n  :hook\n%s" (wisdom-indent hook 2)))
            (when-let* ((init (plist-get package :init)))
              (format "\n  :init\n%s" (wisdom-indent (string-join (mapcar (lambda (x) (wisdom-wrap-in-condition file x)) init)  "\n") 2)))
            (when-let* ((config (plist-get package :config)))
              (format "\n  :config\n%s" (wisdom-indent (string-join (mapcar (lambda (x) (wisdom-wrap-in-condition file x)) config) "\n") 2)))
            (when-let* ((general (plist-get package :general)))
              (format "\n  :general\n%s" (wisdom-indent (string-join (mapcar (lambda (part) (plist-get part :body)) general) "\n") 2)))
            (when-let* ((custom (plist-get package :custom)))
              (format "\n  :custom\n%s" (wisdom-indent (string-join (mapcar (lambda (part) (plist-get part :body)) custom) "\n") 2)))))
   ")\n\n"))

(defun wisdom-build-package (file package-name)
  "Build a `use-package' call for PACKAGE-NAME in string format.
FILE is the file name of the Org file.
PACKAGE-NAME is the name of the package."
  (when-let* ((package (plist-get wisdom-packages package-name)))
    (when (not (equal package-name (intern "nil")))
      (let ((package-string (wisdom-build-package-string package-name package file)))
        (when (wisdom-safe-read package-string file)
          package-string)))))

(defun wisdom-plist-keys (plist)
  "Return the keys of PLIST as a list."
  (let ((keys '()))
    (while plist
      (push (car plist) keys)
      (setq plist (cddr plist)))
    (nreverse keys)))

(defun wisdom-build-packages (file)
  "Build a string of `use-package' statements.
The resulting contains all all packages in `wisdom-packages'.
FILE is the file name of the Org file."
  (let ((package-names (wisdom-plist-keys wisdom-packages))
        (result ""))
    (dolist (package-name package-names)
      (setq result (concat result (wisdom-build-package file package-name))))
    result))

(defun put-package-parameter (package-name parameter value)
  "Put a parameter in the `wisdom-packages' plist.
PACKAGE-NAME is the name of the package.
PARAMETER is the parameter to set.
VALUE is the value to set."
  (setq wisdom-packages
        (plist-put
         wisdom-packages
         package-name
         (plist-put (plist-get wisdom-packages package-name)
                    parameter
                    value))))

(defun wisdom-add-package (package body element)
  "Execute a block of Use-Package code with org-babel.
PACKAGE is a list of the package name and parameter.
BODY is the body of the source block.
ELEMENT is the org element of the source block."
  (let* ((begin (org-element-property :begin element))
         (line (line-number-at-pos begin))
         (package-name (car package))
         (package-parameter (intern (concat ":" (car (cdr package)))))
         (previous-body (plist-get (plist-get wisdom-packages package-name) package-parameter))
         (value (append previous-body `((:body ,body :line ,line)))))
    (put-package-parameter package-name package-parameter value)
    nil))

(defun wisdom-concatenate-source-blocks (file)
  "Concatenate all source blocks in FILE and return the results as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (let ((results '()))
      (org-map-entries
       (lambda ()
         (let ((line (line-number-at-pos (org-element-property :begin (org-element-context)))))
           (when-let* ((package-name (wisdom-find-package))
                       (package (org-element-property :PACKAGE (org-element-context))))
             (put-package-parameter package-name :package line))
           (when-let* ((package-name (wisdom-find-package))
                       (after (wisdom-find-after)))
             (put-package-parameter package-name :after `((:body ,after :line ,line))))
           (when-let* ((package-name (wisdom-find-package))
                       (demand (wisdom-find-demand)))
             (put-package-parameter package-name :demand `((:body ,demand :line ,line))))
           (when-let* ((package-name (wisdom-find-package))
                       (straight (wisdom-find-straight)))
             (put-package-parameter package-name :straight `((:body ,straight :line ,line))))
           (when-let* ((package-name (wisdom-find-package))
                       (defer (wisdom-find-defer)))
             (put-package-parameter package-name :defer `((:body ,defer :line ,line))))
           (when-let* ((package-name (wisdom-find-package))
                       (requires (wisdom-find-requires)))
             (put-package-parameter package-name :requires `((:body ,requires :line ,line)))))))
      (org-babel-map-src-blocks nil
        (let ((body (org-element-property :value (org-element-context)))
              (line (line-number-at-pos (org-element-property :begin (org-element-context))))
              (language (org-element-property :language (org-element-context))))
          (when (string= language "emacs-lisp")
            (if-let* ((package (wisdom-get-use-package-package)))
                (wisdom-add-package package body (org-element-context))
              (when (stringp body)
                (push (wisdom-wrap-in-condition file `(:body ,body :line ,line))
                      results))))))
      (mapconcat 'identity (reverse results) "\n"))))

(defun wisdom-output-file-name (file)
  "Return the name of the output Elisp file for FILE."
  (if (string-prefix-p
       (wisdom-get-org-directory)
       (expand-file-name file))
      (expand-file-name
       (concat (file-name-as-directory (wisdom-get-output-directory))
               (file-name-sans-extension (substring file (length (wisdom-get-org-directory))))
               ".el"))
    (error "File is not in wisdom-org-directory")))

(defun wisdom-compile-file (file)
  "Compile FILE to Elisp.
FILE is an Org file.
The output Elisp file is stored in `wisdom-output-directory'."
  (unless (file-exists-p file)
    (error "File to tangle does not exist: %s" file))
  (unless (file-exists-p (wisdom-get-output-directory))
    (make-directory (wisdom-get-output-directory)))
  (let ((output-file (wisdom-output-file-name file)))
    (make-directory (file-name-directory output-file) t)
    (when (or wisdom-force-compile
              (file-newer-than-file-p file output-file))
      (message "Wisdom: Compiling %s" file)
      (let* ((wisdom-packages nil)
             (source  (wisdom-concatenate-source-blocks file))
             (remote-file-plist (wisdom-file-remote file))
             (output (concat source "\n" (wisdom-build-packages file))))
        (with-temp-file output-file
          (when (not wisdom-compiling-remote)
            (when (wisdom-file-lexical-binding file)
              (insert ";;; -*- lexical-binding: t -*-\n"))
            (when remote-file-plist
              (insert-file-contents (wisdom-remote-plist-to-output-file remote-file-plist)))
            (dolist (property (wisdom-file-properties file))
              (insert (format ";; #+%s: %s\n\n"
                              (upcase (symbol-name (car property)))
                              (cdr property)))))
          (insert output)))
      (when (file-exists-p output-file)
        (set-file-times output-file))
      output-file)))

(defun wisdom-get-files (extension directory)
  "Return all files with EXTENSION in DIRECTORY.
The files are sorted by priority."
  (let* ((files (directory-files-recursively directory extension)))
    (sort files (lambda (a b) (< (wisdom-file-priority a)
                                 (wisdom-file-priority b))))))

(defun wisdom-remote-plist-to-org-file (remote-file-plist)
  (interactive)
  (file-name-concat (file-name-as-directory (expand-file-name wisdom-remote-org-directory))
                    (format "%s" (plist-get remote-file-plist :repo))
                    (format "%s" (plist-get remote-file-plist :file))))

(defun wisdom-remote-plist-to-output-file (remote-file-plist)
  (interactive)
  (file-name-concat (file-name-as-directory (expand-file-name wisdom-remote-output-directory))
                    (format "%s" (plist-get remote-file-plist :repo))
                    (concat (file-name-sans-extension (format "%s" (plist-get remote-file-plist :file))) ".el")))

(defun wisdom-url-retrieve-callback (status remote-file-plist)
  "Callback function for url-retrieve, STATUS contains the request's status."
  (if (plist-get status :error)
      (message "Error status code: %s" (car (last (plist-get status :error))))
    (goto-char url-http-end-of-headers)
    (let ((response-body (buffer-substring-no-properties (point) (point-max)))
          (file-path (wisdom-remote-plist-to-org-file remote-file-plist)))
      (make-directory (file-name-directory file-path) t)
      (with-temp-file file-path
        (insert response-body)))))

(defun wisdom-pull-remote-file (remote-file-plist)
  (interactive)
  (when (or (not (file-exists-p (wisdom-remote-plist-to-org-file remote-file-plist)))
            wisdom-force-download)
    (message "Wisdom: Downloading %s:%s"
             (plist-get remote-file-plist :repo)
             (plist-get remote-file-plist :file))
    (let ((host (or (plist-get remote-file-plist :host) "github"))
          (repo (plist-get remote-file-plist :repo))
          (branch (or (plist-get remote-file-plist :branch) "master"))
          (file (plist-get remote-file-plist :file)))
      (url-retrieve (format "https://raw.githubusercontent.com/%s/refs/heads/%s/%s" repo branch file)
                    (lambda (status &rest remote-file-plist)
                      (wisdom-url-retrieve-callback status remote-file-plist))
                    remote-file-plist))))

(defun wisdom-compile-directory ()
  "Compile all Org files in `wisdom-org-directory' to Elisp.
All files will be outputted to `wisdom-output-directory'."
  (let* ((splash (when wisdom--booting (wisdom-show-splash)))
         (files (wisdom-get-files "^[^#]*\\.org$" (wisdom-get-org-directory)))
         (compiled '())
         (current 0)
         (total (length files)))
    (dolist (file files)
      (setq current (1+ current))
      (when splash
        (wisdom-splash-update-progress splash current total file))
      (when (wisdom-file-remote file)
        (wisdom-pull-remote-file (wisdom-file-remote file)))
      (when-let* ((output (wisdom-compile-file file)))
        (push output compiled)))
    compiled))

(defun wisdom-aggregate-directory (output-file)
  "Aggregate all Org files in `wisdom-org-directory'.
All file contents will be aggregated and outputted to OUTPUT-FILE."
  (let ((result ""))
    (dolist (file (wisdom-get-files "^[^#]*\\.org$" (wisdom-get-org-directory)))
      (setq result (concat result (with-temp-buffer
                                    (insert-file-contents file) (buffer-string)) "\n")))
    (with-temp-file output-file
      (insert result))))

(defun wisdom-load-file (file)
  "Load FILE."
  (let ((inhibit-message t))
    (if (load (expand-file-name file) nil t)
        (message "Wisdom: Loaded %s" file)
      (message "Wisdom: Failed to load %s" file))))

(defun wisdom-load-directory ()
  (let* ((splash (when wisdom--booting (wisdom-show-splash)))
         (initial-gc-cons-threshold gc-cons-threshold)
         (files-local (wisdom-get-files "^[^#]*\\.el$"
                                        (expand-file-name wisdom-output-directory)))
         (files-remote (wisdom-get-files "^[^#]*\\.el$"
                                         (expand-file-name wisdom-remote-output-directory)))
         (files (append files-local files-remote))
         (total (length files))
         (current 0))

    (setq gc-cons-threshold (* 1024 1024 100))

    (dolist (file files)
      (condition-case err
          (progn
            (setq current (1+ current))
            (when splash
              (wisdom-splash-update-progress splash current total file))
            (wisdom-load-file file))
        (error
         (add-to-list 'wisdom--boot-errors
                      (list :file (format "%s" file)
                            :line 1
                            :message (error-message-string err)))
         (unless wisdom--booting
           (display-warning
            'wisdom
            (format "Error loading %s:%s - %s"
                    (format "%s" file)
                    1
                    (error-message-string err))
            :error)))))

    (wisdom-splash-update-progress splash total total nil)

    (setq gc-cons-threshold initial-gc-cons-threshold)
    nil))

(defun wisdom-reload ()
  "Compile and load all Org files."
  (interactive)
  (dolist (compiled-file (wisdom-compile-directory))
    (wisdom-load-file compiled-file)))

(defun wisdom-reload-current-buffer ()
  "Compile and load current Org file."
  (interactive)
  (let ((wisdom-force-compile t)
        (remote-file-plist (wisdom-file-remote (buffer-file-name (current-buffer)))))
    (when remote-file-plist
      (wisdom-pull-remote-file remote-file-plist)
      (let ((wisdom-compiling-remote t))
        (wisdom-compile-file (wisdom-remote-plist-to-org-file remote-file-plist))))
    (when-let* ((compiled-file (wisdom-compile-file (buffer-file-name (current-buffer)))))
      (wisdom-load-file compiled-file))))

(defun wisdom-download-all-remote-files ()
  "Download all remote Org files specified in local Org files."
  (interactive)
  (let ((wisdom-force-download t))
    (dolist (file (wisdom-get-files "^[^#]*\\.org$" (wisdom-get-org-directory)))
      (when-let* ((remote-file-plist (wisdom-file-remote file)))
        (wisdom-pull-remote-file remote-file-plist)))))

(defun wisdom-preview ()
  "Compile the current buffer and display the result in *wisdom preview*."
  (interactive)
  (let* ((buffer (get-buffer-create "*wisdom preview*"))
         ;; We display the buffer first, then compile. If there is an
         ;; error it will not be overruled by the preview buffer.
         (_ (display-buffer buffer))
         (wisdom-wrap-statements-in-condition nil)
         (file (buffer-file-name (current-buffer)))
         (wisdom-packages nil)
         (source (wisdom-concatenate-source-blocks file))
         (output (concat source "\n" (wisdom-build-packages file))))
    (with-current-buffer buffer
      (emacs-lisp-mode)
      (read-only-mode 1)
      (save-excursion
        (let ((inhibit-read-only t))
          (replace-region-contents (point-min) (point-max) (lambda () output)))))))

(define-minor-mode wisdom-preview-mode
  "Preview the current buffer as elisp."
  :lighter " wisdom-preview"
  (if wisdom-preview-mode
      (add-hook 'after-save-hook 'wisdom-preview nil t)
    (remove-hook 'after-save-hook 'wisdom-preview t)))

(defun wisdom-show-splash ()
  (let ((buf (get-buffer-create "*Wisdom Loading*")))
    (with-current-buffer buf
      (erase-buffer)
      (org-mode))
    (switch-to-buffer buf)
    buf))

(defun wisdom-splash-update (buf text)
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (goto-char (point-max))
      (insert (format "%s\n" text))
      (goto-char (point-max)))
    (redisplay)))

(defun wisdom-splash-update-progress (buf current total file)
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (label (pcase wisdom--boot-phase
                     (:compiling "Compiling")
                     (:loading   "Loading")
                     (_          "Processing"))))
        (erase-buffer)
        (insert "WISDOM")
        (insert (make-string 10 ?\n))
        (insert (format "[ %d / %d ]\n\n"
                        current total))
        (insert (wisdom-progress-bar-fancy current total 54))
        (insert "\n\n")
        (if file
            (insert (format "%s: %s\n" label file))
          (insert "\n"))
        (center-region (point-min) (point-max))
        (when wisdom--boot-errors
          (insert "\nErrors encountered:\n\n"))
        (dolist (e wisdom--boot-errors)
          (insert (format "[[%s::%s][%s:%s]]\n"
                          (plist-get e :file)
                          (plist-get e :line)
                          (plist-get e :file)
                          (plist-get e :line)))
          (insert (propertize (format " ⌞ %s\n" (plist-get e :message))
                              'face 'error)))
        (redisplay)
        (unless file
          (org-mode))))))

(defun wisdom-progress-bar-fancy (current total width)
  (let* ((ratio (/ (float current) total))
         (done (floor (* ratio width)))
         (todo (- width done)))
    (format "┌%s┐\n│%s%s│\n└%s┘"
            (make-string width ?─)
            (make-string done ?█)
            (make-string todo ?·)
            (make-string width ?─))))

(defun wisdom-progress-bar (current total width)
  (let* ((ratio (/ (float current) total))
         (done (floor (* ratio width)))
         (todo (- width done)))
    (format "[%s%s]"
            (make-string done ?#)
            (make-string todo ?.))))

(defun wisdom-boot ()
  "Compile and then load all Org files, showing a splash only here."
  (interactive)
  (let ((wisdom--booting t))
    (setq wisdom--boot-errors '())
    (let ((wisdom--boot-phase :compiling))
      (wisdom-compile-directory))

    (let ((wisdom--boot-phase :loading))
      (wisdom-load-directory))))

(provide 'wisdom)

;; TODO Add "push" to loading blocks / files so have an indicator that they're loaded.
;; TODO add #+DISABLED: t
;; TODO Add :ignore to src blocks
;; TODO Allow multiple #+REMOTE: in a single org file
;; TODO Create a lock file for REMOTE dependencies
;; TODO create wisdom-goto-output
;; TODO Conditional inclusion (machine-specific configs, OS-specific blocks, etc.) (#+SYSTEM_TYPE: gnu/linux, darwin)
;;      gnu          compiled for a GNU Hurd system.
;;      gnu/linux    compiled for a GNU/Linux system.
;;      gnu/kfreebsd compiled for a GNU system with a FreeBSD kernel.
;;      darwin       compiled for Darwin (GNU-Darwin, macOS, ...).
;;      ms-dos       compiled as an MS-DOS application.
;;      windows-nt   compiled as a native W32 application.
;;      cygwin       compiled using the Cygwin library.
;;      haiku        compiled for a Haiku system.
;;      android      compiled for Android.
;; TODO Store entire (backtrace) on error?


;;; wisdom.el ends here
