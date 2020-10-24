;;; 50-mail.el --- Mail configurations
;;; Commentary:

;;; Code:

(defvar emacs-data-dir)

(use-package smtpmail
  :config
  ;; SMTP mail setting; these are the same that `gnus' use.
  (use-package message
    :custom
    (message-send-mail-function 'smtpmail-send-it)))

(use-package mu4e
  :custom
  ;; (mu4e-use-fancy-chars t)
  (mail-user-agent 'mu4e-user-agent)
  (mu4e-attachment-dir (expand-file-name "~/Mail/attachments"))
  ;; (mu4e-view-fields '(:date :flags :from :to :tags :subject))
  (mu4e-headers-fields
   '((:human-date . 12)
     (:flags . 6)
     (:from-or-to . 25)
     (:subject)))
  (mu4e-get-mail-command "mbsync -aqq")
  (mu4e-update-interval 300)
  :init
  (add-hook 'after-init-hook 'mu4e-alert-enable-notifications t)
  (add-hook 'after-init-hook 'mu4e-alert-enable-mode-line-display t))

(use-package mu4e-alert
  :config
  (mu4e-alert-set-default-style 'libnotify))

(defconst mail-custom-file (concat emacs-data-dir "mail.el"))

(when (file-readable-p mail-custom-file)
  (load (file-name-sans-extension mail-custom-file)))

;;; 50-mail.el ends here
;;; End:
