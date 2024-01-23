(require 'cl-lib)

(defvar scripture-packages '()
  "List of packages that should be installed.")

(defcustom scripture-use-package-regex "[a-z\\-]+/\\(after\\|straight\\|config\\)"
  "Regular expression to match against use-package parameters."
  :type 'string
  :group 'scripture)

(defvar scripture-package-parameter-rules '(:straight replace :after merge))

(defun scripture-get-use-package-parameter (params)
  (when-let ((filtered
              (seq-filter
               (lambda (x)
                 (string-match-p scripture-use-package-regex
	                         (symbol-name (car x))))
               params)))
    (split-string (symbol-name (car (car filtered))) "/")))


(defun scripture-build-package (package-name)
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
  "Return a list of keys in a property list (plist)."
  (let ((keys '()))
    (while plist
      (push (car plist) keys)
      (setq plist (cddr plist)))
    (nreverse keys)))

(defun scripture-build-packages ()
  (let ((package-names (scripture-plist-keys scripture-packages))
        (result ""))
    (dolist (package-name package-names)
      (setq result (concat result (scripture-build-package package-name))))
    result))

(defun scripture-add-package (package body)
  "Execute a block of Use-Package code with org-babel. This block is treated as Emacs-Lisp code."
  (let* ((package (scripture-get-use-package-parameter params))
         (package-name (intern (car package)))
         (package-parameter (intern (concat ":" (car (cdr package)))))
         (previous-body (plist-get (plist-get scripture-packages package-name) package-parameter))
         (parameter-rule (plist-get scripture-package-parameter-rules package-parameter))
         (new-package-parameter
          (cond
           ( (equal 'replace parameter-rule)
             (plist-put (plist-get scripture-packages package-name)
                        package-parameter body))

           ( (equal 'merge parameter-rule)
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
  "Execute all source blocks in an Org file specified by FILE and capture their results in a string."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (let ((results '()))
      (org-babel-map-src-blocks nil
        (when-let ((result (org-babel-execute-src-block)))
          (push (if (stringp result) result (format "%S" result)) results)))
      (mapconcat 'identity (reverse results) "\n"))))

(defcustom scripture-output-directory "~/.emacs.d/scripture"
  "Directory where the tangled Elisp files are stored."
  :type 'string
  :group 'scripture)

(defcustom scripture-input-directory "~/.dotfiles/emacs/org"
  "Directory where the Org files are stored."
  :type 'string
  :group 'scripture)

(defun scripture-tangled-file-name (file)
  "Return the name of the tangled Elisp file for FILE."
  (concat (file-name-as-directory scripture-output-directory)
          (file-name-nondirectory (file-name-sans-extension file))
          ".el"))

(defun scripture-org-babel-execute:emacs-lisp-shadow (body params)
   (scripture-get-use-package-parameter '())
  (if-let ((package (scripture-get-use-package-parameter params)))
      (scripture-add-package package body)
      body))

(defun scripture-org-babel-compile-file (file)
  (unless (file-exists-p file)
    (error "File to tangle does not exist: %s" file))
  (unless (file-exists-p scripture-output-directory)
    (make-directory scripture-output-directory))
  (let ((tangled-file (scripture-tangled-file-name file)))
    (when (file-newer-than-file-p file tangled-file)
      (cl-letf (((symbol-function 'org-babel-execute:emacs-lisp)
                 (symbol-function 'scripture-org-babel-execute:emacs-lisp-shadow)))
        (let* ((scripture-packages nil)
               (source (scripture-execute-org-src-blocks-and-capture-results file))
               (output (concat source "\n" (scripture-build-packages))))
	  (with-temp-file tangled-file
            (insert output))))
      ;; Make sure that tangled file modification time is
      ;; updated even when `org-babel-tangle-file' does not make changes.
      ;; This avoids re-tangling changed FILE where the changes did
      ;; not affect the tangled code.
      (when (file-exists-p tangled-file)
        (set-file-times tangled-file)))
    (byte-compile-file tangled-file)))

(defun scripture-org-babel-compile-directory (&optional directory)
  (let ((files (directory-files-recursively (or scripture-input-directory directory) "\\.org$")))
    (dolist (file files)
      (scripture-org-babel-compile-file file))))

(defun scripture-load-directory (&optional directory)
  (let ((files (directory-files-recursively (or scripture-output-directory directory) "\\.elc$")))
    (dolist (file files)
      (load-file file))))
