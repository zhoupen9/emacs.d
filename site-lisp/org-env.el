;;; org-env.el --- org-mode related configurations
;;; Commentary:

;;; Code:
             
(use-package org
  :config
  (setq org-directory (expand-file-name "~/Documents"))
  (setq org-default-notes-file (concat org-directory "/Notes/notes.org"))
  (setq org-agenda-files (list "~/Documents/Agenda")))

(use-package org-capture
  :config
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/Documents/Planning/gtd.org" "Tasks")
           "* TODO %?\n %i\n %a")
          ("n" "Note" entry (file+headline "~/Documents/Notes/notes.org" "Notes")
           "* NOTE %?\n %i\n %a")
          ("r" "Reference" entry (file+headline "~/Documents/References/references.org" "References")
           "* REFERENCE %?\n %i\n %a")
          ("j" "Journal" entry (file+datetree "~/Documents/Journal/journal.org")
           "* %?\nEntered on %U\n %i\n %a"))))

(use-package org-re-reveal
  :defines org-re-reveal-root org-re-reveal-revealjs-version
  :config
  (setq org-re-reveal-root "file:///home/pengz/.local/lib/reveal.js")
  (setq org-re-reveal-revealjs-version "4.0.2"))

(use-package org-bullets
  :hook ((org-mode-hook . org-bullets-mode)))

(provide 'org-env)
;;; org-env.el ends here
;;; End:
