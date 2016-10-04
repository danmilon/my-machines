(jedi:setup)

(add-hook 'python-mode-hook (lambda ()
  (hack-local-variables)
  (venv-workon project-venv-name)
  (setq mode-line-format (cons '(:exec venv-current-name) mode-line-format))
  (setq jedi:complete-on-dot t)
  (setq jedi:get-in-function-call-delay 100)
  (jedi-mode)
  (local-set-key (kbd "C-.") 'jedi:goto-definition)
  (local-set-key (kbd "C-TAB") 'jedi:complete)
  (electric-indent-mode -1)
  (local-set-key (kbd "RET") 'newline-and-indent)))

(defun pdb-unittest-current-file ()
  "run pdb on current unittest file"
  (interactive)
  (let ((test-module
    (file-name-sans-extension
     (replace-regexp-in-string
      "/" "." (substring (buffer-file-name) (+ (length project-root) 1))))))
    (pdb (concat "nosetests -s --pdb --pdb-failures " test-module))))

;; j2 is a common jinja 2 extension
;; it might be followed by further extensions
(add-to-list 'auto-mode-alist '("\\.j2.*\\'" . jinja2-mode))
