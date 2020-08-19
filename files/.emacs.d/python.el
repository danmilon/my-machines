(add-hook 'python-mode-hook (lambda ()
  (hack-local-variables)
  (ignore-errors
    (venv-workon project-venv-name)
    (setq mode-line-format (cons '(:exec venv-current-name) mode-line-format)))
  (electric-indent-mode -1)
  (local-set-key (kbd "RET") 'newline-and-indent)
  (pipenv-mode)
  ))

(defun pdb-unittest-current-file ()
  "run pdb on current unittest file"
  (interactive)
  (let ((test-module
    (file-name-sans-extension
     (replace-regexp-in-string
      "/" "." (substring (buffer-file-name) (+ (length project-root) 1))))))
    (pdb (concat "nosetests -s --pdb --pdb-failures " test-module))))
