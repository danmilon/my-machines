(add-hook 'go-mode-hook (lambda ()
  (require 'go-autocomplete)
  (go-eldoc-setup)
  (setq tab-width 4)
  ))
