(add-to-list 'load-path "/usr/share/emacs/site-lisp/")

(setq package-archives '(
  ("gnu"       . "http://elpa.gnu.org/packages/")
  ("melpa"     . "http://melpa.org/packages/")))

(package-initialize)
(package-install 'use-package)

; auto install missing deps with use-package
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; set fill column
(setq-default fill-column 79)

;; Title bar gets in the way when WM is tiling style.
(setq default-frame-alist '((undecorated . t)))

(defun frame-config (frame)
  "Custom behaviour for new frames"
  (when (display-graphic-p frame)
    (tool-bar-mode -1)))

(frame-config (selected-frame))

(add-hook 'after-make-frame-functions 'frame-config)

;; disable menu bar
(menu-bar-mode -1)

;; disable scrollbar
(scroll-bar-mode -1)

;; disable welcome screen
(setq inhibit-startup-message t)

(setq ;; scrolling
  scroll-margin 0                        ;; do smooth scrolling, ...
  scroll-conservatively 100000           ;; ... the defaults ...
  scroll-up-aggressively 0.0             ;; ... are very ...
  scroll-down-aggressively 0.0           ;; ... annoying
  scroll-preserve-screen-position t)     ;; preserve screen pos with C-v/M-v

(delete-selection-mode 1)                ;; delete the sel with a keyp

;; integrate with system clipboard
(setq select-enable-clipboard t)
(setq interprogram-paste-function 'x-selection-value)

(fset 'yes-or-no-p 'y-or-n-p)            ;; enable y/n answers to yes/no

;; No bell.
(customize-set-variable 'ring-bell-function 'ignore)

;; Recenter screen on e.g. grep match visit.
(customize-set-variable 'next-error-recenter '(4))

;; Performance
(setq gc-cons-threshold (* 1000 1000 1000)   ;; 100MB
      read-process-output-max (* 1024 1024)) ;; 1MB

(use-package gcmh
  ;; GC Magit Hack.
  ;; Prevent GC when active, promote it when idle.
  :delight
  :config
  (gcmh-mode))

(use-package yaml-mode)
(use-package web-mode)
(use-package w3)
(use-package json-reformat)
(use-package ace-window)
(use-package delight)
(use-package dockerfile-mode)
(use-package erlang)
(use-package go-eldoc)
(use-package go-mode)
(use-package gruvbox-theme)
(use-package rust-mode)

(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode))

(use-package rainbow-delimiters
  :config
  (rainbow-delimiters-mode))

;; save buffers frequently
(use-package super-save
  :delight
  :config
  ;; add integration with ace-window
  (add-to-list 'super-save-triggers 'ace-window)
  (super-save-mode))

(use-package savehist
  :config
  (savehist-mode))

(use-package eldoc
  :delight)

(use-package pass)

(use-package which-key
  :config
  (which-key-mode))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-M-l")
  :commands (lsp lsp-deferred)
  :hook ((python-mode . lsp)
	 (go-mode . lsp)
	 (c-mode . lsp)
	 (c++-mode . lsp)
	 (rust-mode . lsp)
	 ;; which-key makes it easy to learn the long lsp keybinds.
	 (lsp-mode . lsp-enable-which-key-integration))
  :after (which-key)
  :bind-keymap ("C-M-l" . lsp-command-map)
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.virtualenv\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.virtualenv3\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.eggs\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.gopath\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.tmp\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\__pycache__\\'")

  (setq lsp-prefer-flymake nil
	lsp-auto-guess-root t)

  ;; Golang
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  (setq lsp-gopls-server-path "~/.go/bin/gopls"))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  ;; Disable auto-showing docs. Can still show with lsp-ui-doc-glance.
  (setq lsp-ui-doc-enable nil
	lsp-ui-doc-delay 0))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))

(use-package rustic)

(use-package poetry
  :config
  (poetry-tracking-mode))

;; uniquify: unique buffer names
(use-package uniquify
  :ensure f ;; it's in the standard distribution.
  :config
  (setq
   uniquify-buffer-name-style 'post-forward
   uniquify-separator ":"
   uniquify-after-kill-buffer-p t
   uniquify-ignore-buffers-re "^\\*"))

(global-set-key (kbd "M-g") 'goto-line)

;; load theme
(load-theme 'gruvbox-dark-soft t)

;; when using ido, the confirmation is rather annoying...
 (setq confirm-nonexistent-file-or-buffer nil)

(use-package selectrum
  ;; completion-read enhancement.
  :config
  (selectrum-mode))

(use-package prescient
  ;; completion list filtering sorting enhancement.
  :after selectrum
  :config
  (selectrum-prescient-mode)
  (prescient-persist-mode))

(use-package marginalia
  :after selectrum

  ;; Enable richer annotations using the Marginalia package
  :bind (:map minibuffer-local-map
              ("C-M-a" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode)

  ;; When using Selectrum, ensure that Selectrum is refreshed when cycling annotations.
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit)))))

(use-package ctrlf
  :config
  (ctrlf-mode)
  (customize-set-variable 'ctrlf-default-search-style 'fuzzy))

;; Flyspell
(use-package flyspell
  :hook ((text-mode . flyspell-mode)
	 (prog-mode . flyspell-prog-mode)))

;; realgud
(use-package realgud)

;; lorem ipsum
(use-package lorem-ipsum)

(use-package dired
  :ensure f ;; it's in the standard distribution.
  :config
  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; Omit hidden files.
  (setq dired-omit-mode t)
  (setq dired-omit-files "^\\...+$")
  (setq dired-omit-verbose nil)

  ;; show human readable file sizes
  (customize-set-variable 'dired-listing-switches "-alh")

  ;; enable some really cool extensions like C-x C-j(dired-jump)
  (require 'dired-x))

(use-package ace-window
  :bind (([remap other-window] . 'ace-window)
	 ([remap delete-window] . 'ace-delete-window))
  :config
  (setq aw-scope 'frame))

;; j2 is a common jinja 2 extension
;; it might be followed by further extensions
(use-package jinja2-mode
  :mode "\\.j2.*\\'")

(use-package yasnippet
  :config
  (yas-global-mode 1)
  (yas-load-directory "~/.emacs.d/yasnippets"))
(use-package yasnippet-snippets)

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package flycheck-rust)

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))

(use-package smartparens-config
  :ensure smartparens
  :hook (prog-mode . turn-on-smartparens-strict-mode)
  :config
  (show-smartparens-global-mode t)
  (customize-set-variable 'sp-base-key-bindings 'sp)
  (sp-use-smartparens-bindings))

(use-package lilypond-mode
  :ensure f ;; it's in the standard distribution.
  :mode ("\\.ly\\'" . lilypond-mode))

(use-package go-eldoc
  :hook (ho-mode . go-eldoc-setup)
  :config
  (setq tab-width 4))

(use-package magit
  :bind (("C-c g" . magit-status))
  :config
  (setq magit-diff-refine-hunk 'all))

(use-package markdown-mode
  :mode "\\.md\\'")

(use-package plantuml-mode
  :config
  (custom-set-variables
   '(plantuml-default-exec-mode 'executable)
   '(plantuml 'plantuml-output-type "png")))

(use-package flycheck-plantuml)

(use-package org
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda))
  :config
  (setq org-log-done t)
  (setq org-agenda-files (list "~/Documents/org/work.org"
			       "~/Documents/org/life.org"))
  (setq org-todo-keywords
	'((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

  ;; Do not mess up my windows.
  (setq org-src-window-setup 'current-window)

  ;; plantuml integration via org-babel
  ;; https://eschulte.github.io/babel-dev/DONE-integrate-plantuml-support.html
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((plantuml . t)
     (python . t)
     (sql . t)
     (shell . t)))

  (setq org-plantuml-jar-path
	"/usr/share/java/plantuml.jar")

  ;; Enable org-export backends.
  (custom-set-variables
   '(org-export-backends
     '(ascii
       html
       icalendar
       latex
       md
       odt))))

(use-package ox-gfm
  ;; org export backend to Github Flavored Markdown.
  :after org)

(use-package projectile
    :delight
    :bind-keymap
    ("C-c p" . projectile-command-map)
    :config
    (custom-set-variables
     ;; Keep current project in the list cause out of habbit I always start
     ;; with switch-project.
     '(projectile-current-project-on-switch 'move-to-end)
     ;; When switching projects, show the commander instead of doing find-file.
     '(projectile-switch-project-action #'projectile-commander)
     ;; misc
     '(projectile-enable-caching t)
     '(projectile-use-git-grep 1)
     '(projectile-globally-ignored-directories '(
       ".git"
       "venv"
       ".virtualenv"
       ".tox")))

    (defun my/ad-projectile-project-root (orig-fun &optional dir)
      "This should disable projectile when visiting files with ftp tramp."
      (let ((dir (file-truename (or dir default-directory))))
	(unless (file-remote-p dir)
	  (funcall orig-fun dir))))
    (advice-add 'projectile-project-root :around #'my/ad-projectile-project-root)

    (defun projectile-run-pdb ()
      "Invoke `pdb' in the project's root."
      (interactive)
      (projectile-with-default-dir (projectile-ensure-project (projectile-project-root))
	(call-interactively 'realgud:pdb)))

    (projectile-mode))

;; ansi-colors
(use-package ansi-color
  :mode ("\\.log\\'" . display-ansi-colors)
  :config
  (defun display-ansi-colors ()
    (interactive)
    (ansi-color-apply-on-region (point-min) (point-max)))

  ;; Colors in compilation buffers.
  (defun colorize-compilation-buffer ()
    (ansi-color-apply-on-region compilation-filter-start (point)))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode))

(use-package autorevert
  :delight
  )

(use-package winner
  ;; Winner mode is a global minor mode that records the changes in the window
  ;; configuration (i.e., how the frames are partitioned into windows), so that
  ;; you can undo them.
  :delight
  :config
  (winner-mode))

(use-package multi-term
  :bind (("C-c C-t t" . multi-term)
	 ("C-c C-t n" . multi-term-next)
	 ("C-c C-t p" . multi-term-prev)
	 ("C-c C-t d o" . multi-term-dedicated-open)
	 ("C-c C-t d c" . multi-term-dedicated-close)
	 ("C-c C-t d t" . multi-term-dedicated-toggle)
	 ("C-c C-t d s" . multi-term-dedicated-select))

  :config
  (defun dan/multi-term-switch-buffer (term-buffer default-dir)
    "If we are in `tramp-mode', switch to TERM-BUFFER based on DEFAULT-DIR."
    (switch-to-buffer term-buffer)
    ;; Just test tramp file when library `tramp' is loaded.
    (when (and (featurep 'tramp)
               (tramp-tramp-file-p default-dir))
      (with-parsed-tramp-file-name default-dir path
	(let ((method (cadr (assoc `tramp-login-program (assoc path-method tramp-methods)))))
	  ;; Only change is this message call. but we're not using it? so how does it work now.
	  (message "overcome bug with possibly latest emacs or native-comp which requires the variable 'path' to be used in the macro body otherwise it says the var is void.")
          (term-send-raw-string (concat method " " (when path-user (concat path-user "@")) path-host "\C-m"))
          (term-send-raw-string (concat "cd '" path-localname "'\C-m"))))))

  (advice-add 'multi-term-switch-buffer :override #'dan/multi-term-switch-buffer))

(use-package term
  ;; Also affecting multi-term.
  ;; <2020-03-18 Wed> TODO: These are the default keybinds, why don't they work
  ;; out of the box?
  :bind (("C-c C-k" . term-char-mode)
	 ("C-c C-j" . term-line-mode)))

(use-package forge
  :after magit
  :config
  (add-to-list
   'forge-alist
   '("lab.plat.farm" "lab.plat.farm/api/v4" "lab.plat.farm" forge-gitlab-repository))
  ;; Show maximum 30 OPEN topics in each list in magit status, and no CLOSED.
  (setq forge-topic-list-limit (quote (30 . -1))))

(use-package tramp
  :config
  (customize-set-variable
   'tramp-ssh-controlmaster-options
   (concat
    "-o ControlPath=~/.ssh/sockets/%%C "
    "-o ControlMaster=auto -o ControlPersist=yes"))

  (setq tramp-default-method "ssh")

  ;; Disable backups and lock files in tramp.
  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp nil))
  (customize-set-variable 'remote-file-name-inhibit-locks t)

  (setq remote-file-name-inhibit-cache nil)
  (setq vc-ignore-dir-regexp
	(format "\\(%s\\)\\|\\(%s\\)"
		vc-ignore-dir-regexp
		tramp-file-name-regexp)))

(use-package docker-tramp)

(use-package auth-source
  :config
  (setq auth-sources '((:source "~/.authinfo.gpg"))
	auth-source-save-behavior nil))

(use-package slime
  :config
  (setq inferior-lisp-program "sbcl"))

;; MISC

;; Prefer horizontal window splitting.
(setq split-height-threshold 100)

;; remove trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Fonts & Resizing
;; set font size
(set-face-attribute 'default nil :height 105)

;; font scale keybinds
(defvar text-scale-mode-amount)
(setq text-scale-mode-step 1.05)

;; load  .emacs.d/*.el
(setq load-files-list '("tide" "hydra"))
(dolist (filename load-files-list)
  (load-file (concat "./.emacs.d/" filename ".el")))

;; load psh-el.
(add-to-list 'load-path "~/.emacs.d/emacs-psh/")
(require 'psh)
;; macro to update the VPN auth files.
(fset 'update-psh-vpn
   (kmacro-lambda-form [?\C-s ?< ?c ?a ?\C-m ?\C-n ?\C-a ?\C-  ?\C-s ?< ?/ ?c ?a ?\C-m ?\C-a ?\M-x ?w ?r ?i ?t ?e ?- ?r ?e ?g ?\C-m ?~ ?/ ?. ?c ?e ?r ?t ?/ ?n ?m ?- ?\C-i ?\C-i ?\C-m ?y ?\M-w ?\C-s ?< ?c ?a ?\C-? ?e ?r ?t ?\C-m ?\C-n ?\C-a ?\C-  ?\C-s ?< ?/ ?c ?e ?r ?t ?\C-a ?\C-m ?\C-a ?\M-x ?w ?r ?i ?t ?- ?\C-? ?e ?- ?r ?e ?g ?i ?o ?n ?\C-m ?~ ?/ ?. ?c ?e ?r ?t ?/ ?n ?m ?- ?o ?p ?\C-i ?\C-n ?\C-m ?y ?\M-w ?\C-s ?< ?k ?e ?y ?\C-m ?\C-n ?\C-a ?\C-  ?\C-s ?< ?/ ?k ?e ?y ?\C-m ?\C-a ?\M-x ?w ?r ?i ?t ?- ?r ?e ?g ?i ?o ?\C-? ?\C-? ?\C-? ?\C-? ?\C-? ?\C-? ?e ?- ?r ?e ?g ?i ?o ?n ?\C-m ?~ ?/ ?. ?c ?e ?r ?t ?/ ?n ?m ?= ?- ?\C-? ?\C-? ?- ?\C-i ?k ?e ?y ?\C-m ?y] 0 "%d"))


;; modes for everything
(auto-fill-mode 1)
(electric-indent-mode)
(column-number-mode)

;; Comment Or Uncomment region.
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)

;; Shell mode for ZSH fixes
(when (equal (getenv "SHELL") "/usr/bin/zsh")
  (add-hook
   'shell-mode-hook
   (lambda ()
     (setq comint-input-ring-file-name "~/.zsh_history")
     ; Ignore timestamps in history file. Assumes that zsh EXTENDED_HISTORY
     ; option is in use.
     (setq comint-input-ring-separator "\n: \\([0-9]+\\):\\([0-9]+\\);")
     (comint-read-input-ring t))))

;; In terminal mode, make the window divider prettier (Instead of dashed line,
;; make it solid).
;; See https://www.reddit.com/r/emacs/comments/3u0d0u/how_do_i_make_the_vertical_window_divider_more/
(defun dan/pretty-terminal-window-divider ()
  (unless (display-graphic-p)
    (let ((display-table (or buffer-display-table standard-display-table)))
      (set-display-table-slot display-table 5 ?│)
      (set-window-display-table (selected-window) display-table))))

(add-hook 'window-configuration-change-hook 'dan/pretty-terminal-window-divider)

;; Enable commands that are disabled by default because they're confusing to
;; new users.
(put 'narrow-to-region 'disabled nil)

;; Save bookmarks on every edit, not only on exit.
(customize-set-variable 'bookmark-save-flag 1)

(use-package profiler
  :config
  (setf (caar profiler-report-cpu-line-format) 100
      (caar profiler-report-memory-line-format) 100)
  (setq profiler-max-stack-depth 30))

(use-package ibuffer
  :bind (([remap list-buffers] . ibuffer)))

(use-package ediff
  :config
  (customize-set-variable 'ediff-window-setup-function 'ediff-setup-windows-plain))


(use-package xt-mouse
  ;; It's builtin.
  ;; The original xterm mouse functionality is achieved by holding Shift.
  :config
  (xterm-mouse-mode))

;; TODO: fix this, breaks emacs when running apply() it says not enough args supplied or something.
;; (defun dan/magit-turn-on-auto-revert-mode-if-desired (orig-fun &rest args)
;;   (unless (file-remote-p buffer-file-name)
;;     (apply orig-fun args)))
;; (advice-add 'magit-turn-on-auto-revert-mode-if-desired :before #'dan/magit-turn-on-auto-revert-mode-if-desired)


;; Especially useful for the shell commands history.
(setq history-delete-duplicates t)
(setq comint-input-ignoredups t)

;; Default 100 is too low.
(customize-set-variable 'history-length 1000)

;; TODO: Submit this as an MR (add BECOME to the initial regexp-opt)
(setq comint-password-prompt-regexp
      (rx (or (regexp comint-password-prompt-regexp) "BECOME password:")))

;; TODO: submit patch about process-kill-buffer-query-function prompt text being misleading.
