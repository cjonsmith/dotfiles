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

(load "~/.dotfiles/.emacs.d/newsticker-urls" t)

(setq next-screen-context-lines 10)
(setq frame-resize-pixelwise t)
(setq save-interprogram-paste-before-kill t)

(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta))

(when (eq system-type 'gnu/linux)
  (tool-bar-mode 0))

;; CAUTION: Be sure to reset this to the default value (10) if you're going to be making changes to a file remotely
;; outside of TRAMP or another user has access to the same files and will make changes as well.
(setq remote-file-name-inhibit-cache nil)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package custom-files
  :init (add-to-list 'load-path (directory-file-name custom-library-dir))
  :config
  (global-set-key (kbd "C-c f n") 'cjonsmith/copy-filename-as-kill))

(use-package osrs-shooting-stars
  :init (add-to-list 'load-path (directory-file-name custom-library-dir)))

(use-package graph
  :init (add-to-list 'load-path (directory-file-name custom-library-dir)))

(use-package files
  :after (custom-files)
  :config
  (setq backup-directory-alist (let ((backup-dir-name (cjonsmith/dir-in-emacs-home "backups" nil t)))
				 `(("." . ,backup-dir-name))))
  (setq auto-save-file-name-transforms (let ((auto-save-dir-name (cjonsmith/dir-in-emacs-home "auto-saves" nil t)))
					 `((".*" ,auto-save-dir-name t))))
  (setq lock-file-name-transforms (let ((lock-file-dir-name (cjonsmith/dir-in-emacs-home "lock-files" nil t)))
				    `((".*" ,lock-file-dir-name t)))))

(use-package ibuffer
  :config (global-set-key (kbd "C-x C-b") 'ibuffer))

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

(use-package elisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook
	    (lambda ()
	      (font-lock-add-keywords nil
				      '(("\\<\\(TODO\\|FIXME\\):" 1 font-lock-warning-face t))))))

(use-package files
  :config
  (setq confirm-kill-emacs 'yes-or-no-p))

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

;; Start latest version specific requirements.
(unless (version< emacs-version "28.1")
  (repeat-mode))
;; End latest version specific requirements.

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
 '(Info-quoted ((t (:slant italic))))
 '(mode-line ((t (:background "plum1" :foreground "black" :box (:line-width (2 . 6) :style released-button))))))
