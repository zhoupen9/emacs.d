;;; mail-env.el --- Mail configurations
;;; Commentary:

;;; Code:

(use-package smtpmail
  :config
  ;; SMTP mail setting; these are the same that `gnus' use.
  (use-package message
    :config
    (setq message-send-mail-function   'smtpmail-send-it))
  (setq
   smtpmail-default-smtp-server "smtp.mxhichina.com"
   smtpmail-smtp-server         "smtp.mxhichina.com"
   smtpmail-smtp-service        465
   smtpmail-stream-type         'tls
   smtpmail-mail-address        "zhoup@nroad.com.cn"
   smtpmail-smtp-user           "zhoup@nroad.com.cn"
   smtpmail-local-domain        "mxhichina.com"))

(use-package mu4e
  :config
  ;; "Setup mu4e.
  ;;(setq mu4e-use-fancy-chars t)
  (setq mail-user-agent 'mu4e-user-agent)
  (setq mu4e-attachment-dir (expand-file-name "~/Mail/attachments"))
  (setq user-full-name "Zhou Peng")
  (setq user-mail-address "zhoupen9@sina.cn")
  ;; (setq mu4e-root-maildir (expand-file-name "~/Mail"))
  (setq mu4e-get-mail-command "mbsync -aqq")
  (setq mu4e-update-interval 300)
  :hook
  ((after-init-hook . mu4e-alert-enable-notifications)
   (after-init-hook . mu4e-alert-enable-mode-line-display)))

(use-package mu4e-alert
  :config
  (mu4e-alert-set-default-style 'libnotify))

(provide 'mail-env)
;;; mail-env.el ends here
;;; End:
