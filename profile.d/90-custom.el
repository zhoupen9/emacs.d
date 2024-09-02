;;; -*- lexical-binding: t; -*-
;;; 90-custom.el --- Custom profile configuration
;;; Commentary:

;;; Code:

(use-package ldap-mode)

(defun erase-lsp-bridge-log-buffer ()
  (interactive)
  (save-excursion
    (with-current-buffer (get-buffer-create "*lsp-bridge*")
      (erase-buffer))))

(defun debug-lsp-bridge ()
  (interactive)
  (setq lsp-bridge-enable-log t)
  (desktop-change-dir "~/Workspace")
  (split-window-below)
  (other-window 1)
  (switch-to-buffer "*lsp-bridge*"))

(defun undo-debug-lsp-bridge ()
  (interactive)
  (setq lsp-bridge-enable-log nil)
  (erase-lsp-bridge-log-buffer))

(bind-key "<f9>" 'debug-lsp-bridge)
(bind-key "<C-f9>" 'undo-debug-lsp-bridge)
(bind-key "<f8>" 'erase-lsp-bridge-log-buffer)


;;; 90-custom.el ends here
;;; End:
