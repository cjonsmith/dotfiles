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

;; Enable line numbers, globally
(global-linum-mode 1)

;; Enable Ido, globally
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Update default font to Source Code Pro and disable line spacing
(if (eq system-type 'gnu/linux)
    (set-face-attribute 'default nil :font "Source Code Pro-12")
  )
(setq-default line-spacing 0)

;; Set the size of the frame
(setq-default initial-frame-alist '((width . 135) (height . 55)))

;; Keybindings
(global-set-key (kbd "C-x g") 'magit-status)
(setq mac-command-modifier 'meta)

;; Enable dired-hide-details-mode minor mode in dired mode by default
(add-hook 'dired-mode-hook
	  (lambda ()
	    (dired-hide-details-mode)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-isearch-search t)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(dired-isearch-filenames t)
 '(frame-resize-pixelwise t)
 '(org-startup-truncated nil)
 '(package-selected-packages (quote (flycheck magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
