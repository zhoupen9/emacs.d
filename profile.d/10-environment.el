;;; 10-environment.el --- Basic EMACS Configurations
;;; Commentary:

;;; Code:

(defvar emacs-data-dir)

(customize-set-variable 'abbrev-file-name (concat user-emacs-directory "abbrev_defs"))
(customize-set-variable 'backup-by-copying t)
(customize-set-variable 'backup-by-copying-when-linked t)
(customize-set-variable 'backup-directory-alist `(("." . ,(concat emacs-data-dir "backup/"))))
(customize-set-variable 'auto-save-file-name-transforms
                        `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                           ,(concat emacs-data-dir "auto-save-list/\\2") t)))
(customize-set-variable 'auto-save-list-file-prefix (concat emacs-data-dir "auto-save-list/.saves-"))
(customize-set-variable 'custom-file (concat emacs-data-dir "custom-local.el"))
(customize-set-variable 'display-time-day-and-date t)
(customize-set-variable 'inhibit-startup-message t)
(customize-set-variable 'frame-title-format '("" "[%b] %f - Emacs " emacs-version))
(customize-set-variable 'visible-bell t)
(customize-set-variable 'confirm-kill-emacs 'yes-or-no-p)

;; set 'yes or no' to 'y or n'
(fset 'yes-or-no-p 'y-or-n-p)

;; highlight marked block
(transient-mark-mode t)

;; set tab stop to 4.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;;(setq tab-stop-list (number-sequence 4 120 4))

(when (display-graphic-p)
  ;; Turn off toolbar.
  (tool-bar-mode 0)
  ;; Turn off menubar.
  (menu-bar-mode 0)
  ;; Turn off scroll bar.
  (scroll-bar-mode 0)
  ;; Turn on speed bar.
  ;; (speedbar-frame-mode t)

  ;; Make Noto CJK align with english characters horizontally
  ;; (add-to-list 'face-font-rescale-alist
  ;;               (cons (font-spec :family "Noto Sans CJK SC") 1.2) t)
  ;; Make Noto CJK align with english characters vertically
  ;; (add-to-list 'face-font-rescale-alist
  ;;             (cons (font-spec :family "Noto Sans CJK SC") 0.8) t)

  (when (eq system-type 'gnu/linux)
    ;; Chinese Font
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset
                        (font-spec :family "Noto Sans CJK SC"))))) ;; 中文支持
                        ;;(font-spec :family "Noto Sans CJK SC"))));;abcdefghijk

(use-package bookmark
  :custom
  (bookmark-default-file (concat emacs-data-dir "bookmarks")))

(use-package recentf
  :custom
  (recentf-save-file (concat emacs-data-dir "recentf")))

(use-package esh-mode
  :custom
  (eshell-directory-name (concat emacs-data-dir "eshell/")))

(use-package tramp-cache
  :custom
  (tramp-persistency-file-name (concat emacs-data-dir "tramp")))

(use-package font-core
  :config
  ;; Set font color
  (global-font-lock-mode t))

(use-package time
  ;; :config
  ;; (setq display-time-24hr-format t)
  ;;(setq display-time-day-and-date t)
  :config
  ;; time format
  (display-time))

(use-package simple
  :config
  ;; display line and column number
  (column-number-mode t)
  (line-number-mode t))

(use-package paren
  :config
  ;; show paren
  (show-paren-mode t))

(use-package electric
  :config
  ;; Enable electric minor mode
  (electric-indent-mode t))

(use-package bind-key)

(use-package company
  :config
  (global-company-mode t))

(use-package helm
  :demand
  :config
  (use-package helm-adaptive
    :custom
    (helm-adaptive-history-file (concat emacs-data-dir "helm-adaptive-history")))
  (use-package helm-net
    :config
    (setq helm-net-curl-log-file (concat emacs-data-dir "helm-curl.log")))
  (helm-mode t)
  (helm-projectile-on)
  :bind (("M-x" . helm-M-x)
         ("C-c h b" . helm-buffers-list)))

(use-package yasnippet
  :commands yas-reload-all
  :custom
  (yas-snippet-dirs (list (concat user-emacs-directory "snippets")))
  :config
  (yas-reload-all))

(use-package request
  :custom
  (request-storage-directory (concat emacs-data-dir "request/")))

(use-package transient
  :custom
  (transient-history-file (concat emacs-data-dir "transient/history.el")))

;;; 10-environment.el ends here
;;; End:
