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
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yasnippet yaml-mode which-key wgrep smooth-scroll projectile-ripgrep origami nov mini-frame lsp-mode ido-vertical-mode go-mode forge flycheck fish-mode exec-path-from-shell dumb-jump dracula-theme chess buffer-move browse-at-remote atom-dark-theme async)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
