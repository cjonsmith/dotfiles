;;; init.el --- Cameron Smith's Emacs configuration
;;; Commentary:
;;; Code:
;; Add MELPA (ELisp package archive)
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  )
(package-initialize)

;; Enable syntax checking, globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Disable menu-bar, tool-bar, and scroll-bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Toggle-able line numbers
(global-set-key (kbd "C-x t") 'linum-mode)
(global-set-key (kbd "C-x T") 'global-linum-mode)

;; Enable Ido, globally
(defvar ido-enable-flex-matching)
(defvar ido-everywhere)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Update default font to Source Code Pro and disable line spacing
(if (eq system-type 'gnu/linux)
    (set-face-attribute 'default nil :font "Source Code Pro-12")
  )
(setq-default line-spacing 0)

;; Use GNU ls if on MacOS
(defvar dired-use-ls-dired)
(if (eq system-type 'darwin)
    (setq dired-use-ls-dired t
	  insert-directory-program "/usr/local/bin/gls"))

(setq dired-listing-switches "-lah --group-directories-first")

;; Set the size of the frame
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Keybindings
(global-set-key (kbd "C-x g") 'magit-status)
(setq mac-command-modifier 'meta)

;; Enable dired-hide-details-mode minor mode in dired mode by default
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; Enable normally disabled functions
(put 'narrow-to-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-isearch-search t)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(backup-directory-alist '(("." . "~/.emacs.d/backups")))
 '(custom-enabled-themes '(dracula))
 '(custom-safe-themes
   '("dcdd1471fde79899ae47152d090e3551b889edf4b46f00df36d653adc2bf550d" default))
 '(dired-isearch-filenames t)
 '(frame-resize-pixelwise t)
 '(mini-frame-show-parameters '((left . 0.5) (top . 10) (width . 0.7)))
 '(next-screen-context-lines 10)
 '(org-startup-truncated nil)
 '(package-selected-packages
   '(yasnippet ytdl smooth-scroll mini-frame yaml-mode chess org-drill-table org-drill origami treemacs dracula-theme flycheck magit))
 '(what-cursor-show-names t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; init.el ends here
