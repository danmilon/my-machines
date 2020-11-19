(add-to-list 'load-path "/usr/share/emacs/site-lisp/")

(setq package-archives '(
  ("gnu"       . "http://elpa.gnu.org/packages/")
  ("melpa"     . "http://melpa.org/packages/")))

(package-initialize)

;; set required packages
(setq package-list
      '(
	ace-window
	amx
	autopair
	buffer-move
	concurrent
	counsel
	ctable
	dash
	deferred
	delight
	dired-du
	docker-tramp
	dockerfile-mode
	epc
	epl
	erlang
	expand-region
	flycheck
	flycheck-rust
	git-commit
	gnuplot-mode
	go-eldoc
	go-mode
	google-this
	gruvbox-theme
	ivy
	jinja2-mode
	json-reformat
	lorem-ipsum
	lsp-ivy
	lsp-mode
	lsp-python-ms
	lsp-ui
	lua-mode
	magit
	markdown-mode
	multi-term
	multiple-cursors
	nix-mode
	pipenv
	pkg-info
	pkgbuild-mode
	popup
	projectile
	rainbow-delimiters
	realgud
	restclient
	rust-mode
	sass-mode
	scad-mode
	smartparens
	smooth-scrolling
	super-save
	swiper
	tide
	use-package
	w3
	web-mode
	which-key
	yaml-mode
	yasnippet
	yasnippet-snippets
	zenburn-theme
	))

; install the missing packages
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; set fill column
(setq-default fill-column 79)

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
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

(fset 'yes-or-no-p 'y-or-n-p)            ;; enable y/n answers to yes/no

;; Performance
;; Commented out to test magit diffs.
;; (setq gc-cons-threshold (* 1000 1000 1000)   ;; 100MB
;;       read-process-output-max (* 1024 1024)) ;; 1MB

(use-package gcmh
  ;; GC Magit Hack.
  ;; Prevent GC when active, promote it when idle.
  :delight
  :config
  (gcmh-mode))

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

(use-package which-key
  :config
  (which-key-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((python-mode . lsp)
	 (go-mode . lsp)
	 ;; which-key makes it easy to learn the long lsp keybinds.
	 (lsp-mode . lsp-enable-which-key-integration))
  :after (which-key)
  :config
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

(use-package lsp-ivy
  :after (lsp-ui)
  :bind (([remap lsp-ui-peek-find-workspace-symbol] . lsp-ivy-workspace-symbol)))

(use-package lsp-python-ms
  :hook (python-mode . (lambda () (require 'lsp-python-ms) (lsp))))

;; uniquify: unique buffer names
(use-package uniquify
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

;; amx for smart M-x
(use-package amx
  :config
  (setq amx-backend 'ivy
	amx-show-key-bindings t)
  (amx-mode))

(use-package ivy
  :delight
  :bind (([remap switch-to-buffer] . ivy-switch-buffer)
	 ("C-c C-r" . ivy-resume)
	 ("C-c v" . ivy-push-view)
	 ("C-c V" . ivy-pop-view)
	 :map ivy-mode-map
	 ("C-'" . ivy-avy))
  :config
  (ivy-mode 1)
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  ;; number of result lines to display
  (setq ivy-height 10)
  ;; does not count candidates
  ;; (setq ivy-count-format "")
  ;; no regexp by default
  (setq ivy-initial-inputs-alist nil)
  ;; configure regexp engine.
  (setq ivy-re-builders-alist
	;; allow input not in order
        '((t   . ivy--regex-ignore-order))))

(use-package counsel
  :bind (([remap find-file] . counsel-find-file)
	 ([remap describe-function] . counsel-describe-function)
	 ([remap describe-variable] . counsel-describe-variable)
	 ([remap comint-history-isearch-backward-regexp] . counsel-shell-history)

	 :map minibuffer-local-map
	 ("C-r"     . counsel-minibuffer-history)

	 :map shell-mode-map
	 ("C-r" . counsel-shell-history)))

(use-package swiper
  :bind (
	 ;; There's no notion of forwards/backwards search with swiper.
	 ([remap isearch-forward] . swiper)
	 ([remap isearch-backward] . swiper)))

;; Flyspell
(use-package flyspell
  :hook ((text-mode . flyspell-mode)
	 (prog-mode . flyspell-prog-mode)))

;; realgud
(use-package realgud)

;; lorem ipsum
(use-package lorem-ipsum)

(use-package dired
  :config
  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; Omit hidden files.
  (setq dired-omit-mode t)
  (setq dired-omit-files "^\\...+$")
  (setq dired-omit-verbose nil)

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

(use-package flycheck
  :config
  (global-flycheck-mode))

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
	"/usr/share/java/plantuml/plantuml.jar")

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
    (setq projectile-globally-ignored-directories '(
       ".git"
       "venv"
       ".virtualenv"
       ".tox"))
    ;; When switching projects, show the commander instead of doing find-file.
    (setq projectile-switch-project-action
	  #'projectile-commander)
    (setq projectile-enable-caching t)
    (setq projectile-use-git-grep 1)
    (setq projectile-completion-system 'ivy)

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
	 ("C-c C-t d s" . multi-term-dedicated-select)))

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

  ;; Disable backups in tramp.
  (add-hook 'find-file-hook
            (lambda ()
	      (when (file-remote-p default-directory)
		(setq-local backup-inhibited t))))

  (setq remote-file-name-inhibit-cache nil)
  (setq vc-ignore-dir-regexp
	(format "\\(%s\\)\\|\\(%s\\)"
		vc-ignore-dir-regexp
		tramp-file-name-regexp))

  ;; Disable auto-save backups.
  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp nil))

  ;; Advices to override test functions, but tramp already caches the results
  ;; of those per-connection so it's not too bad.
  ;; (defun psssh-tramp-get-remote-stat (&rest r)
  ;;   "/usr/bin/stat")
  ;; (advice-add 'tramp-get-remote-stat :override 'psssh-tramp-get-remote-stat)
  ;; (defun psssh-tramp-get-remote-readlink (&rest r)
  ;;   "/usr/bin/readlink")
  ;; (advice-add 'tramp-get-remote-readlink :override 'psssh-tramp-get-remote-readlink)
  ;; (defun psssh-tramp-get-test-command (&rest r)
  ;;   "/usr/bin/test")
  ;; (advice-add 'tramp-get-test-command :override 'psssh-tramp-get-test-command)
  ;; (defun psssh-tramp-find-file-exists-command (&rest r)
  ;;   "/usr/bin/test -e")
  ;; (advice-add 'tramp-find-file-exists-command :override 'psssh-tramp-find-file-exists-command)
  ;; (defun psssh-tramp-get-ls-command (&rest r)
  ;;   "/bin/ls")
  ;; (advice-add 'tramp-get-ls-command :override 'psssh-tramp-get-ls-command)
  ;; (defun psssh-tramp-get-ls-command-with-quoting-style (&rest r)
  ;;   t)
  ;; (advice-add 'tramp-get-ls-command-with-quoting-style :override 'psssh-tramp-get-ls-command-with-quoting-style)
  ;; (defun psssh-tramp-get-ls-command-with-dired (&rest r)
  ;;   t)
  ;; (advice-add 'tramp-get-ls-command-with-dired :override (lambda (t)))
  ;; TODO: Possibly use tramp-connection-properties
  )

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
(setq load-files-list '("python" "tide" "hydra"))
(dolist (filename load-files-list)
  (load-file (concat "./.emacs.d/" filename ".el")))

;; load psh-el.
(add-to-list 'load-path "~/.emacs.d/emacs-psh/")
(require 'psh)

;; modes for everything
(auto-fill-mode 1)
(electric-indent-mode)
(column-number-mode)
(rainbow-delimiters-mode)
(smooth-scrolling-mode)

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

;; Bookmark+
(let ((bookmarkplus-dir "~/.emacs.d/custom/bookmark-plus/")
      (emacswiki-base "https://www.emacswiki.org/emacs/download/")
      (bookmark-files '("bookmark+.el" "bookmark+-mac.el" "bookmark+-bmu.el" "bookmark+-key.el" "bookmark+-lit.el" "bookmark+-1.el")))
  (require 'url)
  (add-to-list 'load-path bookmarkplus-dir)
  (make-directory bookmarkplus-dir t)
  (mapcar (lambda (arg)
            (let ((local-file (concat bookmarkplus-dir arg)))
              (unless (file-exists-p local-file)
                (url-copy-file (concat emacswiki-base arg) local-file t))))
          bookmark-files)
  (byte-recompile-directory bookmarkplus-dir 0)
  (require 'bookmark+))

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

;; Especially useful for the shell commands history.
(setq history-delete-duplicates t)
(setq comint-input-ignoredups t)

;; Default 100 is too low.
(customize-set-variable 'history-length 500)

;; TODO: Submit this as an MR (add BECOME to the initial regexp-opt)
(setq comint-password-prompt-regexp
      (rx (or (regexp comint-password-prompt-regexp) "BECOME password:")))
