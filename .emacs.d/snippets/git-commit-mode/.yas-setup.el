;;; .yas-setup.el --- Helper functions for the Co-authored-by snippet.
;;; Commentary:
;;; Code:
(defun co-author-name (co-author-handle)
  "Return the name of a co-author found on their GitHub account given their `CO-AUTHOR-HANDLE'."
  (gethash "name" (json-parse-string
		   (with-current-buffer (url-retrieve-synchronously
					 (concat "https://api.github.com/users/" co-author-handle)
					 t)
		     (prog2
			 (kill-paragraph 1)
			 (buffer-string)
		       (kill-buffer))))))
;;; .yas-setup.el ends here
