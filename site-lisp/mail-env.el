;;; mail-env.el --- Mail configurations
;;; Commentary:

;;; Code:

(use-package smtpmail
  :config
  ;; SMTP mail setting; these are the same that `gnus' use.
  (use-package message
    :config
    (setq message-send-mail-function   'smtpmail-send-it))
  (when (string= (system-name) "zrs-comp-gz97i7a")
    (setq
     smtpmail-default-smtp-server "smtp.sina.cn"
     smtpmail-smtp-server         "smtp.sina.cn"
     smtpmail-smtp-service        465
     smtpmail-stream-type         'tls
     smtpmail-mail-address        "zhoupen9@sina.cn"
     smtpmail-smtp-user           "zhoupen9@sina.cn"
     smtpmail-local-domain        "sina.cn"))
  (when (string= (system-name) "xanr-dev-pri-wcmi")
      (setq
   smtpmail-default-smtp-server "smtp.mxhichina.com"
   smtpmail-smtp-server         "smtp.mxhichina.com"
   smtpmail-smtp-service        465
   smtpmail-stream-type         'tls
   smtpmail-mail-address        "zhoup@nroad.com.cn"
   smtpmail-smtp-user           "zhoup@nroad.com.cn"
   smtpmail-local-domain        "mxhichina.com")))

(use-package mu4e
  :config
  ;; "Setup mu4e.
  ;;(setq mu4e-use-fancy-chars t)
  (setq
   mail-user-agent 'mu4e-user-agent
   mu4e-attachment-dir (expand-file-name "~/Mail/attachments")
   user-full-name "Zhou Peng"
   ;; mu4e-view-fields '(:date :flags :from :to :tags :subject)
   mu4e-headers-fields
   '((:human-date . 12)
     (:flags . 6)
     (:from-or-to . 25)
     (:subject))
   mu4e-get-mail-command "mbsync -aqq"
   mu4e-update-interval 300)

  (when (string= (system-name) "zrs-comp-gz97i7a")
    (setq user-mail-address "zhoupen9@sina.cn"))
  (when (string= (system-name) "xanr-dev-pri-wcmi")
    (setq user-mail-address "zhoup@nroad.com.cn"))

  ;; (setq mu4e-root-maildir (expand-file-name "~/Mail"))
  :init
  (add-hook 'after-init-hook 'mu4e-alert-enable-notifications t)
  (add-hook 'after-init-hook 'mu4e-alert-enable-mode-line-display t))

(use-package mu4e-alert
  :config
  (mu4e-alert-set-default-style 'libnotify))

(provide 'mail-env)
;;; mail-env.el ends here
;;; End:
