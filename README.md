# Wisdom

Wisdom is an Emacs package that provides a structured and flexible way
to manage your Emacs configuration using Org mode. By leveraging Org
mode's hierarchical structure, Wisdom allows you to organize your
Emacs Lisp configuration in a modular and readable format. Unlike
traditional Org-based configurations, which are limited to a single
file and a single src block per `use-package`, Wisdom supports
multiple Org files, remote configurations, and distributed
`use-package` declarations.

## Features

- **Modular Configuration with Org Mode**: Organize your Emacs
  configuration using Org mode's headings, properties, and source
  blocks for a clean and maintainable setup.
- **Distributed `use-package` Blocks**: Define multiple `use-package`
  statements across different source blocks within a single package,
  with support for keywords like `:init`, `:config`, `:bind`, and
  more.
- **Error Handling with Try/Catch**: Automatically wraps source blocks
  in `condition-case` forms, including line numbers for easier
  debugging.
- **Remote Org File Support**: Incorporate remote Org files from
  repositories (e.g., GitHub) to use or share configurations
  seamlessly.
- **Priority-Based Loading**: Control the order in which Org files are
  compiled and loaded using the `#+PRIORITY` property.
- **Lexical Binding Control**: Enable or disable lexical binding per Org
  file with the `#+LEXICAL_BINDING` property.
- **Preview Mode**: Preview the compiled Emacs Lisp output of your Org
  file in real-time with `wisdom-preview-mode`.
- **Customizable Directories**: Configure separate directories for local
  and remote Org files and their compiled outputs.
- **Flexible Compilation**: Force compilation or downloading of files
  with customizable options like `wisdom-force-compile` and
  `wisdom-force-download`.

## Installation

To use Wisdom, you need to set up your Emacs environment with `straight.el` for package management and ensure the latest version of Org mode is installed. Below is a minimal `init.el` configuration to get started.

```emacs-lisp
;; Bootstrap straight.el
(defvar bootstrap-version)
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
  (load bootstrap-file nil 'nomessage))

;; Install Org mode
(straight-use-package 'org)

;; Install and configure Wisdom
(use-package wisdom
  :straight (wisdom :type git :host github :repo "kwrooijen/wisdom")
  :custom
  (wisdom-org-directory "~/.emacs.d/org")
  (wisdom-output-directory "~/.emacs.d/wisdom")
  (wisdom-remote-org-directory "~/.emacs.d/remote-org")
  (wisdom-remote-output-directory "~/.emacs.d/remote-wisdom")
  :config
  (wisdom-boot))
#+END_SRC

This configuration:
1. Installs `straight.el` for package management.
2. Installs the latest version of Org mode.
3. Installs Wisdom from its GitHub repository.
4. Configures the directories for Org files and compiled outputs.
5. Compiles and loads all Org files in the specified directory.

** Usage

Wisdom allows you to write your Emacs configuration in one or more Org files, using Org mode's structure to define `use-package` declarations and other Emacs Lisp code. Below is an example of how to structure an Org file for Wisdom.

*** Example Org File

#+BEGIN_SRC org
#+TITLE: My Emacs Configuration
#+PRIORITY: 5
#+LEXICAL_BINDING: t

* Package: evil
:PROPERTIES:
:PACKAGE: evil
:STRAIGHT: t
:DEFER: t
:END:

** Init
#+BEGIN_SRC emacs-lisp
(setq evil-want-keybinding nil)
#+END_SRC

** Config
#+BEGIN_SRC emacs-lisp
(evil-mode 1)
#+END_SRC

** Bind
#+BEGIN_SRC emacs-lisp
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
#+END_SRC

* Remote Configuration
#+REMOTE: (:host github :repo "kwrooijen/test-org" :branch main :file key-chord.org)

* General Emacs Settings
#+BEGIN_SRC emacs-lisp
(setq inhibit-startup-message t)
(tool-bar-mode -1)
#+END_SRC
```

In this example:
- The `#+PRIORITY: 5` sets the loading priority of the file.
- The `#+LEXICAL_BINDING: t` enables lexical binding.
- The `evil` package is configured with `use-package`, using Org properties (`:PACKAGE`, `:STRAIGHT`, `:DEFER`) and headings for keywords (`init`, `config`, `bind`).
- A remote Org file is included using the `#+REMOTE` property.
- General Emacs settings are defined in a separate source block.

*** Compiling and Loading

To compile and load your configuration:
1. Save your Org files in `wisdom-org-directory` (default: `~/.emacs.d/org`).
2. Run `M-x wisdom-compile-directory` to compile all Org files to Emacs Lisp.
3. Run `M-x wisdom-load-directory` to load the compiled files.

Alternatively, use `M-x wisdom-reload` to compile and load in one step.

To compile and load the current Org file, use `M-x wisdom-reload-current-buffer`.

*** Previewing Output

To preview the compiled Emacs Lisp output of the current Org file:
1. Run `M-x wisdom-preview` to display the output in the `*wisdom preview*` buffer.
2. Enable `wisdom-preview-mode` to automatically update the preview after saving the file.

*** Managing Remote Files

To download remote Org files specified in `#+REMOTE` properties:
- Run `M-x wisdom-download-all-remote-files` to fetch all remote files.
- Wisdom caches remote files in `wisdom-remote-org-directory` and compiles them to `wisdom-remote-output-directory`.

** Configuration Options

Wisdom provides several customizable variables to tailor its behavior. These can be set in your `init.el` or an Org file.

#+BEGIN_SRC emacs-lisp
(use-package wisdom
  :custom
  ;; Directory for local Org files
  (wisdom-org-directory "~/.emacs.d/org")
  ;; Directory for compiled local Elisp files
  (wisdom-output-directory "~/.emacs.d/wisdom")
  ;; Directory for remote Org files
  (wisdom-remote-org-directory "~/.emacs.d/remote-org")
  ;; Directory for compiled remote Elisp files
  (wisdom-remote-output-directory "~/.emacs.d/remote-wisdom")
  ;; Wrap source blocks in condition-case for error handling
  (wisdom-wrap-statements-in-condition t)
  ;; Force compilation even if Elisp file is newer
  (wisdom-force-compile nil)
  ;; Force download of remote Org files
  (wisdom-force-download nil)
  ;; Supported use-package keywords
  (wisdom-use-package-keywords
   '("after" "straight" "config" "init" "bind" "bind_" "hook" "general" "custom")))
#+END_SRC

** Org File Properties

Wisdom supports the following Org file properties to control compilation and loading:

- `#+PRIORITY: <number>`: Sets the loading priority (default: 10). Lower numbers load earlier.
- `#+LEXICAL_BINDING: t|nil`: Enables or disables lexical binding (default: t).
- `#+REMOTE: (:host <host> :repo <repo> :branch <branch> :file <file>)`: Specifies a remote Org file to include.

** Debugging

Wisdom enhances debugging by:
- Wrapping each source block in a `condition-case` form, which catches errors and displays the file name and line number.
- Providing detailed warnings for syntax errors or invalid Emacs Lisp forms.
- Allowing preview of the compiled output to inspect the generated Emacs Lisp code.

** Contributing

Contributions to Wisdom are welcome! To contribute:
1. Fork the repository at `https://github.com/kwrooijen/wisdom`.
2. Create a new branch for your changes.
3. Submit a pull request with your improvements.

** License

Wisdom is licensed under the MIT License. See the LICENSE file in the repository for details.

** TODOs

The following features are planned for future releases:
- Add try/catch wrapping for entire Org files.
- Implement a loading indicator for blocks and files.
- Support `#+DISABLED: t` to skip specific files.
- Add `:ignore` support for source blocks.
- Allow multiple `#+REMOTE` properties in a single Org file.
- Create a lock file for remote dependencies.
- Implement `wisdom-goto-output` to jump to compiled output files.
