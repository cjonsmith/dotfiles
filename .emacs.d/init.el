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
(set-face-attribute 'default nil :font "Source Code Pro-12")
(setq-default line-spacing 0)

;; Set the size of the frame
(setq-default initial-frame-alist '((width . 135) (height . 55)))
