;; TODO: make it get the absolute root path "/" of the remote python-cwd
(defun tramp-get-root-path ()
  "Get the root path of the remote, with the full tramp path."
  (let ((tramp-structure (tramp-dissect-file-name default-directory)))
    (setcar (nthcdr 6 tramp-structure) "/")
    (tramp-make-tramp-file-name tramp-structure)))

(defun python-shell--save-temp-file (string)
  "Put (as STRING) in a temporary file, modified to understand remote tramp hosts."
  (let* ((python-cwd (with-current-buffer (python-shell-get-buffer) (tramp-get-root-path)))
	 (temporary-file-directory
	  (if (file-remote-p python-cwd)
	      (concat python-cwd "/tmp")
	    temporary-file-directory))
         (temp-file-name (make-temp-file "py"))
         (coding-system-for-write (python-info-encoding)))
    (with-temp-file temp-file-name
      (insert string)
      (delete-trailing-whitespace))
    temp-file-name))

(defun platform-shell ()
  (interactive)
  (run-python "platform shell" nil t))
