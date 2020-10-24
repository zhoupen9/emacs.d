;;; 70-news.el --- org-mode related configurations
;;; Commentary:

;;; Code:
             
(use-package elfeed-org
  :custom
  (rmh-elfeed-org-files (list (concat user-emacs-directory "elfeed.org")))
  :config
  ;; Initialize elfeed-org
  ;; This hooks up elfeed-org to read the configuration when elfeed
  ;; is started with =M-x elfeed=
  (elfeed-org)
  ;; Optionally specify a number of files containing elfeed
  ;; configuration. If not set then the location below is used.
  ;; Note: The customize interface is also supported.
  :init
  (use-package elfeed))

;;; 70-news.el ends here
;;; End:
