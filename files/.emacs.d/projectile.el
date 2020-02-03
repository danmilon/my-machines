;; projectile
(use-package projectile
    :bind-keymap
    ("C-c p" . projectile-command-map)
    :config
    (setq projectile-globally-ignored-directories '(
       ".git"
       "venv"
       ".virtualenv"
       ".tox"))
    (setq projectile-enable-caching t)
    (setq projectile-use-git-grep 1)
    (projectile-mode))
