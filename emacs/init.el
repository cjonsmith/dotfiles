 ;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes '(modus-vivendi-tritanopia))
 '(custom-file "~/dotfiles/emacs/init.el")
 '(display-line-numbers t)
 '(display-line-numbers-grow-only t)
 '(fido-mode t)
 '(fido-vertical-mode t)
 '(frame-resize-pixelwise t)
 '(global-display-line-numbers-mode t)
 '(global-visual-line-mode t)
 '(mouse-wheel-progressive-speed nil)
 '(next-screen-context-lines 5)
 '(ns-command-modifier 'meta)
 '(package-selected-packages '(go-mode))
 '(tab-bar-history-mode t)
 '(tab-bar-mode t)
 '(tab-width 4)
 '(winner-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-[") 'tab-bar-history-back)
(global-set-key (kbd "M-]") 'tab-bar-history-forward)
(windmove-default-keybindings)
(put 'narrow-to-region 'disabled nil)
