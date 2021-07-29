;;; 50-org.el --- org-mode related configurations
;;; Commentary:

;;; Code:

(defvar emacs-data-dir)
             
(use-package org
  :demand
  :custom
  (org-hide-emphasis-markers t)
  (org-directory (expand-file-name "~/Documents"))
  (org-default-notes-file (concat org-directory "/Notes/notes.org"))
  (org-agenda-files '("~/Documents/Planning/gtd.org"
                      "~/Documents/Planning/someday.org"
                      "~/Documents/Planning/tickler.org"))
  (org-refile-targets '(("~/Documents/Planning/gtd.org" :maxlevel . 3)
                      ("~/Documents/Planning/someday.org" :maxlevel . 2 )
                      ("~/Documents/Planning/tickler.org" :maxlevel . 2))))

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
  :custom
  (org-re-reveal-root (concat "file:///" (expand-file-name "~/.local/lib/reveal.js")))
  (org-re-reveal-revealjs-version "4.0.2"))

(use-package org-bullets
  :demand
  :commands org-bullets-mode
  :after (org)
  :hook (org-mode . org-bullets-mode))

(use-package org-journal
  :init
  ;; Change default prefix key; needs to be set before loading org-journal
  (setq org-journal-prefix-key "C-c j ")
  :custom
  (org-journal-dir "~/Documents/Journal")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-date-format "%A, %d %B %Y"))

(use-package org-roam
  :after helm
  :init
  (setq org-roam-v2-ack t)
  :bind (("C-c r o" . org-roam)
         ("C-c r f" . org-roam-node-find)
         ("C-c r r" . org-roam-ref-find)
         ("C-c r i" . org-roam-node-insert))
  :custom
  (org-roam-directory "~/Documents/Notes")
  (org-roam-completion-system 'helm)
  (org-roam-db-location (concat emacs-data-dir "org-roam.db3"))
  (org-id-locations-file (concat emacs-data-dir "org-id-locations"))
  :config
  (add-hook 'after-init-hook 'org-roam-mode))

;;; 50-org.el ends here
;;; End:
