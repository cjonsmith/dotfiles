;;; init.el --- Cameron Smith's Emacs configuration
;;; Commentary:
;;; Code:
;; Add MELPA (ELisp package archive) and Org package archive.
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "org" (concat proto "://orgmode.org/elpa/")) t))
(package-initialize)

;; Custom library configuration variable initialization and loading of custom libraries found at and beneath
;; the specified custom-library-dir.
(defvar custom-library-dir (concat user-emacs-directory "custom")
  "Location of where to load custom/personal libraries from.")

(push (directory-file-name custom-library-dir) load-path)

(require 'osrs-shooting-stars)
;; End custom library configuration.

(load "~/.dotfiles/.emacs.d/newsticker-urls" t)

(setq next-screen-context-lines 10)
(setq frame-resize-pixelwise t)
(setq save-interprogram-paste-before-kill t)

(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta))

(when (eq system-type 'gnu/linux)
  (tool-bar-mode 0))

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c f n") 'cjonsmith-copy-filename-as-kill)

(defun get-ghes-releases ()
  "Fetches the current releases of GHES.

Returns an list of alists whose car is a minor version of GHES and whose cdr is its latest patch version."
  (with-current-buffer
      (url-retrieve-synchronously "https://github-enterprise.s3.amazonaws.com/release/latest.json")
    (goto-char url-http-end-of-headers)
    (let ((json-key-type 'string))
      (json-read))))

(defun ghes-releases ()
  "Shows the most recent minor version of each major version of GHES."
  (interactive)
  (with-output-to-temp-buffer "*GHES Releases*"
      (temp-buffer-resize-mode)
      (mapcar (lambda (version-alist)
		(princ (format "%s: %s\n" (car version-alist) (cdr version-alist))))
	      (get-ghes-releases))))

;; Begin graph related functions from chapter 15 of Emacs Lisp Intro
(defvar graph-symbol "*"
  "String used as symbol in graph, usually an asterisk.")

(defvar graph-blank " "
  "String used as blank in graph, usually a blank space.
graph-blank must be the same number of columns wide
as graph-symbol.")

(defvar graph-max-height 20
  "The maximum height a graph column should be.")

(defun graph-body-print (numbers-list)
  "Insert a body of a graph at point, whose column values are the contents of `NUMBERS-LIST'.

The max value of a column should be scaled to is defined as `graph-max-height'.  The largest value
of `NUMBERS-LIST' will be translated to that value, and all other values will be translated by the
same amount to avoid overly large columns from displaying."
  (let ((max-height (apply 'max numbers-list)))
    (while numbers-list
    ;; Draw column.
      (insert-rectangle
       (column-of-graph graph-max-height (translate-height max-height (car numbers-list))))

    ;; Reposition point.
      (forward-line (- graph-max-height))
      (move-end-of-line nil)

      ;; Shrink numbers-list.
      (setq numbers-list (cdr numbers-list)))))

(defun column-of-graph (max-graph-height actual-height)
  "Returns a list of strings representation of a column of a graph of length `MAX-GRAPH-HEIGHT'.

`ACTUAL-HEIGHT' is the number of graph-symbols that will appear in the column. Graph symbols are represented as the value
of the variable `graph-symbol', and the empty filler spaces are represented as the value of `graph-blank'."
  (let ((insert-list nil)
	(number-of-top-blanks (- max-graph-height actual-height)))
    ;; Fill in asterisks.
    (while (> actual-height 0)
      (setq insert-list (cons graph-symbol insert-list))
      (setq actual-height (1- actual-height)))

    ;; Fill in blanks.
    (while (> number-of-top-blanks 0)
      (setq insert-list (cons graph-blank insert-list))
      (setq number-of-top-blanks (1- number-of-top-blanks)))

    ;; Return whole list.
    insert-list))

(defun translate-height (max-height actual-height)
  "Given an untranslated height of `ACTUAL-HEIGHT', scale it down to fit within the bounds of `graph-max-height'."
  ;; If unset, default graph-max-height to 20.
  (unless graph-max-height
    (setq graph-max-height 20))
  (truncate (* (/ (float graph-max-height) max-height) actual-height)))
;; End graph related functions from chapter 15 of Emacs Lisp Intro

;; TODO Add a function to copy the path of the current buffer's file (and optionally line number) to kill-ring.
(defun cjonsmith-copy-filename-as-kill ()
  "Adds the filename (including path) of the current buffer to the kill ring.

If called with C-u, then only copy the name of the file."
  (interactive)
  (cond
   ((equal current-prefix-arg nil)
    (kill-new (buffer-file-name)))
   ((equal current-prefix-arg '(4))
    (kill-new (file-name-nondirectory (buffer-file-name))))))

;; CAUTION: Be sure to reset this to the default value (10) if you're going to be making changes to a file remotely
;; outside of TRAMP or another user has access to the same files and will make changes as well.
(setq remote-file-name-inhibit-cache nil)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(use-package project
  :ensure project)

(setq scroll-conservatively 1)

(use-package dired
  :config
  (setq dired-dwim-target t))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

(use-package project
  :ensure project
  :after (exec-path-from-shell)
  :config
  (if (executable-find "rg")
      (setq xref-search-program 'ripgrep)
    (message "Executable: `ripgrep' was not found in `exec-path'.  Ensure that it is installed on your system if you wish to speed-up xref")))

(use-package winner
  :config
  (setq winner-mode 1))

(use-package which-key
  :config
  (which-key-mode))

(use-package org
  :ensure org
  :config
  (progn
    ;; Default all non-accounted for file-types to open in Emacs (I might regret this later)
    (add-to-list 'org-file-apps '(t . emacs) t)
    (setq org-image-actual-width nil)))

(use-package sh-script
  :config
  (progn
    (setq sh-basic-offset 2)
    (add-hook 'sh-mode-hook
              (lambda ()
                (setq indent-tabs-mode nil)))))

(use-package ruby-mode
  :after lsp-mode)

(use-package go-mode
  :after lsp-mode)

(use-package forge
  :after magit
  :init
  (add-hook 'magit-status-sections-hook #'forge-insert-assigned-issues))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-enable-snippet nil)
  :hook
  ((sh-mode . lsp)
   (ruby-mode . lsp)
   (go-mode . lsp)
   (typescript-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration)))

(use-package terraform-mode)

(when (not (eq system-type 'darwin))
  (use-package company
    :ensure company
    :custom
    (company-minimum-prefix-length 1)
    (company-idle-deplay 0.0)
    :hook
    (sh-mode . company-mode))

  (use-package company-shell
    :after (company)
    :config (add-to-list 'company-backends 'company-shell)))

(use-package doc-view
  :init
  (setq doc-view-resolution 400))

(use-package request
  :ensure request)

(use-package nov
  :mode (".epub" . nov-mode)
  :config
  (progn
    (defun my-nov-font-setup ()
      (face-remap-add-relative 'variable-pitch
			       :family "Cochin Regular"
			       :height 1.5))
    (add-hook 'nov-mode-hook 'my-nov-font-setup)))

(use-package docker
  :bind ("C-c d" . docker))

(use-package yafolding
  :hook
  ((sh-mode . yafolding-mode))
  :config
  (defvar yafolding-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "<C-S-return>") #'yafolding-hide-parent-element)
      (define-key map (kbd "<C-M-return>") #'yafolding-toggle-all)
      (define-key map (kbd "<C-return>") #'yafolding-toggle-element)
      map)))

(use-package orgit)

(use-package eshell
  :init
  (setq eshell-visual-subcommands '(("docker" "load"))))

(use-package graphviz-dot-mode
  :ensure t
  :config
  (setq graphviz-dot-indent-width 4))

(use-package codespaces
  :config
  (if (executable-find "gh")
      (codespaces-setup)
    (message "Executable: `gh' was not found in `exec-path'.  Ensure that it is installed on your system if you wish to use GitHub Codespaces."))
  :bind ("C-c S" . #'codespaces-connect))

;; In my experience, MacOS lacks any system default libraries that `hunspell' (the default spellchecker that comes with MacOS) can
;; access.  This may be a little heavy-handed to solve that problem, but by installing `aspell' it will also include several
;; different dictionaries along side the binary.  Switching to using `aspell' seems to be the quickest/least manual way of solving
;; this problem, since `brew' (or whatever package manager that's being used) will handle setting the `PATH' environment variable
;; correctly (thus including it in the `exec-path' variable in Emacs) along with setting up the dictionaries for it as well.
(if (executable-find "aspell")
    (setq ispell-program-name "/usr/local/bin/aspell")
  (message "Executable: `aspell' was not found in `exec-path'.  Ensure that it is installed on your system if you wish to use spellchecking."))

;; Allow narrowing by default.
(put 'narrow-to-region 'disabled nil)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cal-html-year-index-cols 4)
 '(org-clock-sound t)
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(package-selected-packages
   '(codespaces terraform-mode graphviz-dot-mode orgit yafolding docker dockerfile-mode typescript-mode request company-shell company project use-package yasnippet yaml-mode which-key wgrep smooth-scroll projectile-ripgrep origami nov mini-frame lsp-mode ido-vertical-mode go-mode forge flycheck fish-mode exec-path-from-shell dumb-jump dracula-theme chess buffer-move browse-at-remote atom-dark-theme async))
 '(tramp-histfile-override nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
