;; magit
(use-package magit
  :bind (("C-c g" . magit-status))
  :config
  (setq magit-diff-refine-hunk 'all))
