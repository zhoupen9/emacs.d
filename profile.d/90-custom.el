;;; 90-custom.el --- Custom profile configuration
;;; Commentary:

;;; Code:

(use-package ldap-mode)

(defun debug-lsp-bridge ()
  (interactive)
  (setq lsp-bridge-enable-log t)
  (desktop-change-dir "~/Workspace")
  (split-window-below)
  (other-window 1)
  (switch-to-buffer "*lsp-bridge*"))

(bind-key "<f9>" 'debug-lsp-bridge)

(bind-key "<f8>" (lambda ()
                   (interactive)
                   (save-excursion
                     (with-current-buffer (get-buffer-create "*lsp-bridge*")
                       (erase-buffer)))))

;;; 90-custom.el ends here
;;; End:
