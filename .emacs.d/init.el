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

;;; YASnippet configurations
;; Enable YASnippet
(require 'yasnippet)
(yas-global-mode)

;; Since `git-commit-mode' is only a minor mode, YASnippet won't load the git-commit-mode
;; snippet table.  We can invoke the `yas-activate-extra-mode' function in a hook when the
;; minor mode `git-commit-mode' is invoked, forcing YASnippet to load the desired snippet
;; table.
(add-hook 'git-commit-mode-hook
	  #'(lambda ()
	      (yas-activate-extra-mode 'git-commit-mode)))

;; YASnippet also seems to have an issue with finding a snippet directory if it is a
;; symlink, which is how I have my dotfiles configured.  Adding a path directly to the
;; dotfiles directory seems to circumvent this issue.
(setq yas-snippet-dirs (append yas-snippet-dirs
			       '("~/.dotfiles/.emacs.d/snippets")))
;;; End YASnippet configuration.

;;; nov configuration
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(defun my-nov-font-setup ()
  "Hook to set a new face for nov mode."
  (face-remap-add-relative 'variable-pitch :family "Serif"
			   :height 1.0))
(add-hook 'nov-mode-hook 'my-nod-font-setup)
;;; End nov configuration

;;; ripgrep configuration
;; Search in hidden directories and rely on contents of `projectile-globally-ignored-directories' to exclude
;; undesired hidden directories.
(defvar ripgrep--base-arguments)
(add-to-list 'ripgrep--base-arguments "--hidden")
;;; End ripgrep configuration

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
(add-to-list 'default-frame-alist
 	     '(font . "Source Code Pro for Powerline-14"))
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

;; Enable some minor modes
(which-key-mode t)
(ido-vertical-mode t)

;;; Projectile configuration
(projectile-mode)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(defvar projectile-project-search-path)
(setq projectile-project-search-path '("~/github" "~/Software"))
;;; End Projectile configuration.

;;; exec-path-from-shell configuration
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(when (daemonp)
  (exec-path-from-shell-initialize))
;;; End exec-path-from shell configuration

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
   '("e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" "dcdd1471fde79899ae47152d090e3551b889edf4b46f00df36d653adc2bf550d" default))
 '(dired-isearch-filenames t)
 '(frame-resize-pixelwise t)
 '(mini-frame-show-parameters '((left . 0.5) (top . 10) (width . 0.7)))
 '(next-screen-context-lines 10)
 '(org-startup-truncated nil)
 '(package-selected-packages
   '(nov exec-path-from-shell projectile-ripgrep projectile browse-at-remote doom-themes yasnippet ytdl smooth-scroll ido-vertical-mode mini-frame yaml-mode chess org-drill-table org-drill origami treemacs dracula-theme flycheck magit))
 '(what-cursor-show-names t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; init.el ends here
