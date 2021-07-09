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

(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta))

(defun get-ghes-releases ()
  "Fetches the current releases of GHES.

Returns an list of alists whose car is a minor version of GHES and whose cdr is its latest patch version."
  (with-current-buffer
      (url-retrieve-synchronously "https://github-enterprise.s3.amazonaws.com/release/latest.json")
    (goto-char url-http-end-of-headers)
    (let ((json-key-type 'string))
      (json-read))))

(defun ghes-releases ()
  "Shows the current versions the available version of GHES."
  (interactive)
  (with-output-to-temp-buffer "*GHES Releases*"
      (temp-buffer-resize-mode)
      (mapcar (lambda (version-alist)
		(princ (format "%s: %s\n" (car version-alist) (cdr version-alist))))
	      (get-ghes-releases))))

;; TODO Add a function to copy the path of the current buffer's file (and optionally line number) to kill-ring.

(require 'use-package)
(use-package project)

(use-package which-key
  :config
  (which-key-mode))

(use-package org
  :ensure org
  :config
  (progn
    ;; Default all non-accounted for file-types to open in Emacs (I might regret this later)
    (add-to-list 'org-file-apps '(t . emacs) t)))

(use-package sh-script
  :config
  (progn
    (add-hook 'sh-mode-hook
              (lambda ()
                (setq indent-tabs-mode nil)))))

(use-package forge
  :after magit
  :init
  (add-hook 'magit-status-sections-hook #'forge-insert-assigned-issues))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  ((sh-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration)))

(use-package company
  :ensure company
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-deplay 0.0)
  (global-company-mode t))

(use-package company-shell
  :after (company)
  :config (add-to-list 'company-backends 'company-shell))

(use-package doc-view
  :init
  (setq doc-view-resolution 400))

(use-package request
  :ensure request)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(request company-shell company project use-package yasnippet yaml-mode which-key wgrep smooth-scroll projectile-ripgrep origami nov mini-frame lsp-mode ido-vertical-mode go-mode forge flycheck fish-mode exec-path-from-shell dumb-jump dracula-theme chess buffer-move browse-at-remote atom-dark-theme async)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
