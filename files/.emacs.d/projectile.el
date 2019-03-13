;; projectile
(setq projectile-keymap-prefix (kbd "C-c C-p"))
(setq projectile-globally-ignored-directories '(
   ".git"
   "venv"
   ".virtualenv"
   ".tox"))


(setq projectile-enable-caching t)
(setq projectile-use-git-grep 1)
(projectile-mode)
