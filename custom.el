;;; custom.el --- Emacs customization file
;;; Commentary:

;;; Code:

(custom-set-variables
 '(display-time-day-and-date t)
 '(tab-stop-list (number-sequence 4 120 4))
 '(language-environtment "UTF-8")
 '(backup-directory-alist '(("" . "~/.emacs.d/backup")))
 '(abbrev-file-name "~/.emacs.d/.abbrev_defs")
 '(speedbar-mode-hook 'variable-pitch-mode))

;; custom rtags
(custom-set-variables
 '(rtags-completions-enabled t)
 '(rtags-display-result-backend 'helm))

;; custom helm
(custom-set-variables
 '(helm-M-x-fuzzy-match t))

;; custom flycheck
(custom-set-variables
 '(flycheck-emacs-lisp-load-path 'inherit))
 
;; custom jedi
(custom-set-variables
 '(jedi:complete-on-dot t)
 '(jedi:tooltip-method nil)
 '(jedi:get-in-function-call-delay 0))

;;; End:
;;; custom.el ends here
