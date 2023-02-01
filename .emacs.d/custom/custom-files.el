;; TODO: Add a function to copy the path of the current buffer's file (and optionally line number) to kill-ring.
(defun cjonsmith/copy-filename-as-kill ()
  "Adds the filename (including path) of the current buffer to the kill ring.

If called with C-u, then only copy the name of the file."
  (interactive)
  (cond
   ((equal current-prefix-arg nil)
    (kill-new (buffer-file-name)))
   ((equal current-prefix-arg '(4))
    (kill-new (file-name-nondirectory (buffer-file-name))))))

(defun cjonsmith/dir-in-emacs-home (dir &optional exists create)
  "Appends name of DIR to emacs home path and returns it as a dir.

Optional non-nil EXISTS argument ensures that the directory is only returned if it exists, otherwise `nil' is returned.
Optional non-nil CREATE arguments ensures that the directory does exist when function is invoked. When both EXISTS and
CREATE are non-nil values, EXISTS takes precedence (i.e. the directory will not be created if it doesn't already exist."
  (let ((dir-in-emacs-home (directory-file-name
			    (concat user-emacs-directory dir))))
    (cond
     (exists
      (when (file-directory-p dir-in-emacs-home)
	dir-in-emacs-home))
     (create
      (progn
	(files--ensure-directory dir-in-emacs-home)
	dir-in-emacs-home))
     (t
      dir-in-emacs-home))))

(defun cjonsmith/open-user-emacs-directory (open-init)
  "Opens dired at `user-emacs-directory' in current window.  Opens `init.el' at that directory if OPEN-INIT is non-nil.

If called interactively, the presence of a prefix argument is equivalent to setting OPEN-INIT to non-nil."
  (interactive "P")
  (if open-init
      (find-file (locate-user-emacs-file "init.el"))
    (dired user-emacs-directory)))

(provide 'custom-files)
