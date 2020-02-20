(add-to-list 'load-path "/usr/share/emacs/site-lisp/")

(setq package-archives '(
  ("gnu"       . "http://elpa.gnu.org/packages/")
  ("melpa"     . "http://melpa.milkbox.net/packages/")))

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
	flx-ido
	flycheck
	flycheck-rust
	git-commit
	gnuplot-mode
	go-eldoc
	go-mode
	google-this
	ido
	ivy
	ido-vertical-mode
	jinja2-mode
	json-reformat
	lorem-ipsum
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
	tide
	use-package
	virtualenvwrapper
	w3
	web-mode
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
(setq gc-cons-threshold (* 1000 1000 1000)   ;; 100MB
      read-process-output-max (* 1024 1024)) ;; 1MB

;; save buffers frequently
(use-package super-save
  :delight
  :config
  ;; add integration with ace-window
  (add-to-list 'super-save-triggers 'ace-window)
  (super-save-mode +1))

(use-package eldoc
  :delight)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((python-mode . lsp))
  :config
  (setq lsp-prefer-flymake nil
	lsp-auto-guess-root t))

(use-package lsp-ui
  :hook ((lsp-mode . lsp-ui-mode))
  :config
  ;; Disable auto-showing docs. Can still show with lsp-ui-doc-glance.
  (setq lsp-ui-doc-enable nil
	lsp-ui-doc-delay 0)
  :bind (("s-l h g" . lsp-ui-doc-glance)))

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
(load-theme 'zenburn t)

;; ido - https://www.emacswiki.org/emacs/InteractivelyDoThings
(use-package ido
  :config
  (setq ido-max-prospects 8                  ; don't spam my minibuffer
	ido-case-fold  t                    ; be case-insensitive
	ido-enable-last-directory-history t ; remember last used dirs
	ido-max-work-directory-list 30      ; should be enough
	ido-max-work-file-list      50      ; remember many
	ido-use-filename-at-point nil       ; don't use filename at point (annoying)
	ido-use-url-at-point nil            ; don't use url at point (annoying)
	ido-enable-flex-matching nil        ; don't try to be too smart
	ido-max-prospects 8                 ; don't spam my minibuffer
	ido-confirm-unique-completion t)    ; wait for RET, even with unique completion
  (ido-mode t)
  (ido-everywhere t)
  (ido-vertical-mode 1))

;; flx-ido - https://github.com/lewang/flx
(use-package flx-ido
  :config
  (flx-ido-mode 1)
  ;; disable ido faces to see flx highlights
  (setq ido-use-faces nil))

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
  :bind
  (:map ivy-mode-map
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
  :bind (("C-x C-f" . counsel-find-file)
	 ("C-h f" . counsel-describe-function)
	 ("C-h v" . counsel-describe-variable)))

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

  ;; enable some really cool extensions like C-x C-j(dired-jump)
  (require 'dired-x))

(use-package ace-window
  :bind (("C-x o" . 'ace-window))
  :config
  (setq aw-scope 'frame))

;; j2 is a common jinja 2 extension
;; it might be followed by further extensions
(use-package jinja2-mode
  :mode "\\.j2.*\\'")

(use-package yasnippet
  :config
  (yas-global-mode 1)
  (yas-load-directory ".emacs.d/yasnippets"))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package restclient-mode
  :mode "\\.http\\'")

;; ansi-colors
(use-package ansi-color
  :mode ("\\.log\\'" . display-ansi-colors)
  :config
  (defun display-ansi-colors ()
    (interactive)
    (ansi-color-apply-on-region (point-min) (point-max))))

(use-package linum
  :hook (prog-mode . linum-mode))

;; MISC

;; remove trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Fonts & Resizing
;; set font size
(set-face-attribute 'default nil :height 105)

;; font scale keybinds
(defvar text-scale-mode-amount)
(setq text-scale-mode-step 1.05)

(define-globalized-minor-mode
  global-text-scale-mode
  text-scale-mode
  (lambda () (text-scale-mode 1)))

(defun global-text-scale-adjust (inc) (interactive)
  (text-scale-set 1)
  (kill-local-variable 'text-scale-mode-amount)
  (setq-default text-scale-mode-amount (+ text-scale-mode-amount inc))
  (global-text-scale-mode 1))

(global-set-key [C-mouse-4] '(lambda () (interactive)
			       (global-text-scale-adjust 1)))
(global-set-key [(control ?+)] '(lambda () (interactive)
				  (global-text-scale-adjust 1)))
(global-set-key [C-mouse-5] '(lambda () (interactive)
			      (global-text-scale-adjust -1)))
(global-set-key [(control ?-)] '(lambda () (interactive)
				 (global-text-scale-adjust -1)))
(global-set-key (kbd "C-0") '(lambda () (interactive)
			      (global-text-scale-adjust
			       (- text-scale-mode-amount))
			      (global-text-scale-mode -1)))


;; load  .emacs.d/*.el
(setq load-files-list '(
  "python" "scala" "org" "smartparens" "multi-term" "lilypond" "tramp"
  "markdown" "restclient" "projectile" "golang" "tide" "magit"))

(dolist (filename load-files-list)
  (load-file (concat "./.emacs.d/" filename ".el")))

;; modes for everything
(auto-fill-mode 1)
(electric-indent-mode)
(column-number-mode)
(rainbow-delimiters-mode)
(smooth-scrolling-mode)

;; Comment Or Uncomment region.
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
