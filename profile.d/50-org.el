;;; 50-org.el --- org-mode related configurations
;;; Commentary:

;;; Code:

(defvar emacs-data-dir)
             
(use-package org
  :demand
  :bind
  (("C-c i s" . org-insert-structure-template))
  :custom
  (org-hide-emphasis-markers t))

(use-package org-bullets
  :demand
  :commands org-bullets-mode
  :after (org)
  :hook (org-mode . org-bullets-mode))

(use-package markdown-mode
  :custom
  (markdown-fontify-code-blocks-natively t))

;;; 50-org.el ends here
;;; End:
