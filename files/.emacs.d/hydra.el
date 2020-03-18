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
    (("h" windmove-left "left")
     ("j" windmove-down "down")
     ("k" windmove-up "up")
     ("l" windmove-right "right")
     ("o" other-window "next"))

    "Manage"
    (("v" split-window-right "split vertically")
     ("x" split-window-below "split horizontally")
     ("m" delete-other-windows "maximize")
     ("s" ace-swap-window "swap")
     ("a" ace-select-window "select"))

    "Move Divider"
    (("C-h" hydra-move-splitter-left "left")
     ("C-j" hydra-move-splitter-down "down")
     ("C-k" hydra-move-splitter-up "up")
     ("C-l" hydra-move-splitter-right "right"))

    "Winner"
    (("u" winner-undo "undo")
     ("r" winner-redo "redo")))))
