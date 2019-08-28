(add-to-list 'load-path "/usr/share/emacs/site-lisp/")

(setq package-archives '(
  ("gnu"       . "http://elpa.gnu.org/packages/")
  ("melpa"     . "http://melpa.milkbox.net/packages/")))

(package-initialize)

;; set required packages
(setq package-list
      '(
	ace-jump-mode
	ace-window
	auto-complete
	autopair
	buffer-move
	concurrent
	ctable
	dash
	deferred
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
	go-autocomplete
	go-eldoc
	go-mode
	google-this
	ido
	ido-vertical-mode
	jedi
	jinja2-mode
	json-reformat
	lorem-ipsum
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
	smex
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

(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

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

;; save buffers frequently
(super-save-mode)

;; uniquify: unique buffer names
(require 'uniquify) ;; make buffer names more unique
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":"
  uniquify-after-kill-buffer-p t
  uniquify-ignore-buffers-re "^\\*")

(global-set-key (kbd "M-g") 'goto-line)

;; load theme
(load-theme 'zenburn t)

;; ido/flx-ido
(ido-mode 1)
(ido-everywhere 1)
(setq
 ido-max-prospects 8                  ; don't spam my minibuffer
  ido-case-fold  t                    ; be case-insensitive
  ido-enable-last-directory-history t ; remember last used dirs
  ido-max-work-directory-list 30      ; should be enough
  ido-max-work-file-list      50      ; remember many
  ido-use-filename-at-point nil       ; don't use filename at point (annoying)
  ido-use-url-at-point nil            ; don't use url at point (annoying)
  ido-enable-flex-matching nil        ; don't try to be too smart
  ido-max-prospects 8                 ; don't spam my minibuffer
  ido-confirm-unique-completion t)    ; wait for RET, even with unique completion

;; when using ido, the confirmation is rather annoying...
 (setq confirm-nonexistent-file-or-buffer nil)

;; magit
(global-set-key (kbd "C-c g") 'magit-status)

(ido-vertical-mode 1)

(flx-ido-mode 1)

;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

;; smex
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; text modes
(add-hook 'text-mode-hook (lambda ()
  (auto-fill-mode 1)
  (flyspell-mode 1)))

;; libnotify fun
(defun notify (title msg &optional icon sound)
  "Show a popup if we're on X, or echo it otherwise; TITLE is the title
of the message, MSG is the context. Optionally, you can provide an ICON and
a sound to be played"

  (interactive)
  (when sound (shell-command
                (concat "mplayer -really-quiet " sound " 2> /dev/null")))
  (if (eq window-system 'x)
    (shell-command (concat "notify-send "

                     (if icon (concat "-i " icon) "")
                     " '" title "' '" msg "'"))
    ;; text only version

    (message (concat title ": " msg))))


;; lorem ipsum
(autoload 'Lorem-ipsum-insert-paragraphs "lorem-ipsum" "" t)
(autoload 'Lorem-ipsum-insert-sentences "lorem-ipsum" "" t)
(autoload 'Lorem-ipsum-insert-list "lorem-ipsum" "" t)

;; dired enhancements
;; load dired-x when dired is loaded
(add-hook 'dired-load-hook '(lambda () (require 'dired-x)))
(setq dired-omit-mode t)
(setq dired-omit-files "^\\...+$")

;; remove trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; yasnippet
(yas-global-mode 1)
(yas-load-directory ".emacs.d/yasnippets")

;; enable flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; common modes for programming
(add-hook 'prog-mode-hook (lambda ()
  (auto-fill-mode 1)
  (electric-indent-mode)
  (linum-mode)
  (column-number-mode)
  (flyspell-prog-mode)
  (auto-complete-mode 1)
  (rainbow-delimiters-mode)
  (smooth-scrolling-mode)
  (load-library "realgud")
  ))

;; ansi-colors
(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(add-to-list 'auto-mode-alist '("\\.log\\'" . display-ansi-colors))

(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)

;; ace-window
(global-set-key (kbd "C-x o") 'ace-window)

;; ace-jump-mode
(global-set-key (kbd "C-c j c") 'ace-jump-char-mode)
(global-set-key (kbd "C-c j w") 'ace-jump-word-mode)
(global-set-key (kbd "C-c j l") 'ace-jump-line-mode)

;; load  .emacs.d/*.el
(setq load-files-list '(
  "python" "scala" "org" "smartparens" "multi-term" "lilypond" "tramp"
  "markdown" "restclient" "projectile" "golang" "tide" "magit"))

(dolist (filename load-files-list)
  (load-file (concat "./.emacs.d/" filename ".el")))
