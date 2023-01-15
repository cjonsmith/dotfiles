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

(provide 'ghes-releases)
