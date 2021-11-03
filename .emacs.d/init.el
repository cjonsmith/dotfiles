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

(load "~/.dotfiles/.emacs.d/newsticker-urls" t)

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

(setq osrs-shooting-stars-locations
      '((0 . "Asgarnia")
	(1 . "Karamja or Crandor")
	(2 . "Feldip Hills or Isle of Souls")
	(3 . "Fossil Island or Mos Le'Harmless")
	(4 . "Fremnik Lands or Lunar Isle")
	(5 . "Great Kourend")
	(6 . "Kandarin")
	(7 . "Kebos Lowlands")
	(8 . "Kharidian Desert")
	(9 . "Misthalin")
	(10 . "Morytania")
	(11 . "Piscatoris or the Gnome Stronghold")
	(12 . "Tirannwn")
	(13 . "Wilderness")
	(14 . "Unknown")))

(defun osrs-get-shooting-stars ()
  "Fetches the current known list of shooting stars in Old School RuneScape."
  (let ((url-request-extra-headers
	 '(("Authorization" . "global"))))
    (with-current-buffer
	(url-retrieve-synchronously "https://z9smj03u77.execute-api.us-east-1.amazonaws.com/stars")
      (goto-char url-http-end-of-headers)
      (let ((json-key-type 'string))
	(json-read)))))

(defun osrs-shooting-star-world (star_data)
  "Returns the world a shooting star is on given a STAR_DATA object.

STAR_DATA is single object from the JSON array that is returned from a call to the `osrs-get-shooting-stars'
function."
  (cdr (car (cdr star_data))))

(defun osrs-shooting-star-location (star_data)
  "Returns the location a shooting star is on given a STAR_DATA object.

STAR_DATA is a single object from the JSON arry that is returned from a call to the `osrs-get-shooting-stars'
function."
  (cdr (assq (cdr (car star_data)) osrs-shooting-stars-locations)))

(defun osrs-shooting-star-min-time (star_data)
  "Returns the minimum time in minutes until a shooting star spawns given a STAR_DATA object.

STAR_DATA is a single object from the JSON arry that is returned from a call to the `osrs-get-shooting-stars'
function."
  (/ (subtract-time
      (cdr (car (cdr (cdr star_data))))
      (time-convert (current-time) 'integer))
     60))

(defun osrs-shooting-star-max-time (star_data)
  "Returns the maximum time in minutes until a shooting star spawns given a STAR_DATA object.

STAR_DATA is a single object from the JSON arry that is returned from a call to the `osrs-get-shooting-stars'
function."
  ( / (subtract-time
       (cdr (car (cdr (cdr (cdr star_data)))))
       (time-convert (current-time) 'integer))
      60))

(defun osrs-shooting-stars (minutes)
  "Opens a temporary buffer containing a list of worlds and locations of future shooting stars in Old School Runescape within the last MINUTES."
  (interactive "p")
  (with-output-to-temp-buffer "*OSRS Shooting Stars*"
    (temp-buffer-resize-mode)
    (message "Minutes: %d" minutes)
    (mapcar (lambda (location)
	      (let ((world (osrs-shooting-star-world location))
		    (location (osrs-shooting-star-location location))
		    (min-time (osrs-shooting-star-min-time location))
		    (max-time (osrs-shooting-star-max-time location)))
		(when (>= max-time (* -1 minutes))
		  (princ (format "World %d: %s (%d minutes - %d minutes)\n"
				 world
				 location
				 min-time
				 max-time)))))
	    (osrs-get-shooting-stars))))

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

;; In my experience, MacOS lacks any system default libraries that `hunspell' (the default spellchecker that comes with MacOS) can
;; access.  This may be a little heavy-handed to solve that problem, but by installing `aspell' it will also include several
;; different dictionaries along side the binary.  Switching to using `aspell' seems to be the quickest/least manual way of solving
;; this problem, since `brew' (or whatever package manager that's being used) will handle setting the `PATH' environment variable
;; correctly (thus including it in the `exec-path' variable in Emacs) along with setting up the dictionaries for it as well.
(if (executable-find "aspell")
    (setq ispell-program-name "/usr/local/bin/aspell")
  (message "Executable: `aspell' was not found in `exec-path'.  Ensure that it is installed on your system if you wish to use spellchecking."))

(setq scroll-conservatively 1)

(require 'use-package)
(use-package project
  :ensure project)

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
    (add-to-list 'org-file-apps '(t . emacs) t)))

(use-package sh-script
  :config
  (progn
    (setq sh-basic-offset 2)
    (add-hook 'sh-mode-hook
              (lambda ()
                (setq indent-tabs-mode nil)))))

(use-package ruby-mode
  :after lsp-mode)

(use-package forge
  :after magit
  :init
  (add-hook 'magit-status-sections-hook #'forge-insert-assigned-issues))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  ((sh-mode . lsp)
   (ruby-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration)))

(use-package company
  :ensure company
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-deplay 0.0)
  :hook
  (sh-mode . company-mode))

(use-package company-shell
  :after (company)
  :config (add-to-list 'company-backends 'company-shell))

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
