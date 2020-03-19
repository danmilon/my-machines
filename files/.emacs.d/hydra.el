(use-package hydra
  :ensure t)

(use-package pretty-hydra
  :ensure t
  :after (hydra))

(defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

(global-set-key
 (kbd "C-M-o")
 (pretty-hydra-define hydra-window (:foreign-keys warn :quit-key "q")
   ("Navigate"
    (("C-f" windmove-right "right")
     ("C-b" windmove-left "left")
     ("C-n" windmove-down "down")
     ("C-p" windmove-up "up")
     ("o" other-window "next"))

    "Manage"
    (("3" split-window-right "split vertically")
     ("2" split-window-below "split horizontally")
     ("M-3" (lambda ()
	    (interactive)
	    (split-window-right)
	    (windmove-right)) "split & go horizontally")
     ("M-2" (lambda ()
	    (interactive)
	    (split-window-below)
	    (windmove-down)) "split & go vertically")
     ("m" delete-other-windows "maximize")
     ("s" ace-swap-window "swap")
     ("a" ace-select-window "select")
     ("da" ace-delete-window "select & delete")
     ("dw" delete-window "delete current"))

    "Move Divider"
    (("M-f" hydra-move-splitter-right "right")
     ("M-b" hydra-move-splitter-left "left")
     ("M-n" hydra-move-splitter-down "down")
     ("M-p" hydra-move-splitter-up "up"))

    "Winner"
    (("u" (lambda ()
	    (interactive)
	    (winner-undo)
	    (setq this-command 'winner-undo)) "undo")
     ("r" winner-redo "redo")))))
