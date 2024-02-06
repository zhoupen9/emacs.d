;;; 90-custom.el --- Custom profile configuration
;;; Commentary:

;;; Code:

(use-package ldap-mode)

(defun debug-lsp-bridge ()
  (interactive)
  (desktop-change-dir "~/Workspace")
  (split-window-below)
  (other-window 1)
  (switch-to-buffer "*lsp-bridge*"))

(bind-key "<f9>" 'debug-lsp-bridge)
;;; 90-custom.el ends here
;;; End:
