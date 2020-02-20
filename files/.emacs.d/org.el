(use-package org
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda))
  :config
  (setq org-log-done t)
  (setq org-agenda-files (list "~/Documents/org/work.org"
			       "~/Documents/org/life.org"))
  (setq org-todo-keywords
	'((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

  ;; plantuml integration via org-babel
  ;; https://eschulte.github.io/babel-dev/DONE-integrate-plantuml-support.html
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((plantuml . t)
     (python . t)))

  (setq org-plantuml-jar-path
	"/usr/share/java/plantuml/plantuml.jar"))
