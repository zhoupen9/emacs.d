;;; site-start.el --- Site start
;;; Commentary:

;;; Code:

;;(doom-modeline-mode t)

;;Initialize exec-path-from-shell
;; (use-package exec-path-from-shell
;;   :config
;;   (exec-path-from-shell-initialize)
;;   (setq exec-path-from-shell-check-startup-files nil))

(use-package base-env)

(use-package org-env)

(use-package mail-env
    :config
  ;; key bindings
  ;; Bind key <F12> to open 'mu4e'
  (global-set-key (quote [f12]) (quote mu4e)))

(use-package news-env)

(use-package prog-env)

;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "C-c r f") 'rtags-find-symbol)
;; (global-set-key (kbd "C-c r c") 'rtags-find-symbol-at-point)
;; (global-set-key (kbd "C-c r p") 'rtags-print-symbol-info)
;; (global-set-key (kbd "C-c r b") 'rtags-location-stack-back)

(use-package pui
  :hook
  ((org-mode-hook . set-org-buffer-variable-pitch)
   (markdown-mode-hook . set-markdown-buffer-variable-pitch))
  :config
  ;; binding key control-\ to comment/uncomment.
  (global-set-key (kbd "C-c l") 'comment-or-uncomment-region-or-line)
  ;; binding key control-c control-e to beautify.
  (global-set-key (kbd "C-c b f") 'beautify))

;; load spacemacs dark theme
(load-theme 'spacemacs-dark t)

(server-start)

;;; site-start.el ends here
;;; End:
