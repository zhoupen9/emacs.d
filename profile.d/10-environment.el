;;; 10-environment.el --- Basic EMACS Configurations
;;; Commentary:

;;; Code:

(defvar emacs-data-dir)

;; Customize variables
(custom-set-variables
 '(abbrev-file-name (concat user-emacs-directory "abbrev_defs"))
 '(backup-by-copying t)
 '(backup-by-copying-when-linked t)
 '(backup-directory-alist `(("." . ,(concat emacs-data-dir "backup/"))))
 '(auto-save-file-name-transforms
   `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
      ,(concat emacs-data-dir "auto-save-list/\\2") t)))
 '(auto-save-list-file-prefix (concat emacs-data-dir "auto-save-list/.saves-"))
 '(custom-file (concat emacs-data-dir "custom-local.el"))
 '(inhibit-startup-message t)
 '(frame-title-format '("" "[%b] %f - Emacs " emacs-version))
 '(visible-bell t)
 '(tramp-persistency-file-name (concat emacs-data-dir "tramp"))
 '(native-comp-async-report-warnings-errors nil)
 '(confirm-kill-emacs 'yes-or-no-p))

;; set 'yes or no' to 'y or n'
(fset 'yes-or-no-p 'y-or-n-p)

;; highlight marked block
(transient-mark-mode t)

;; turon-off display time
(display-time-mode 0)

;; set tab stop to 4.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default desktop-path (concat emacs-data-dir "desktop"))
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
  :demand
  :custom
  (tramp-persistency-file-name (concat emacs-data-dir "tramp")))

(use-package font-core
  :config
  ;; Set font color
  (global-font-lock-mode t))

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

(use-package savehist
  :init
  (savehist-mode))

(use-package vertico
  :demand
  :init
  (vertico-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

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

(use-package url
  :custom
  (url-configuration-directory (concat emacs-data-dir "url")))

;; (defun corfu-enable-always-in-minibuffer ()
;;   "Enable Corfu in the minibuffer if Vertico/Mct are not active."
;;   (unless (or (bound-and-true-p mct--active)
;;               (bound-and-true-p vertico--input))
;;     ;; (setq-local corfu-auto nil) Enable/disable auto completion
;;     (corfu-mode 1)))

;; (defun corfu-enable-in-minibuffer ()
;;   "Enable Corfu in the minibuffer if `completion-at-point' is bound."
;;   (when (where-is-internal #'completion-at-point (list (current-local-map)))
;;     ;; (setq-local corfu-auto nil) Enable/disable auto completion
;;     (corfu-mode 1)))

(use-package corfu
  :init
  (global-corfu-mode)
  :bind ("C-." . completion-at-point)
  :custom
  ;;(corfu-auto t)
  (tab-always-indent 'complete))
  ;; :config
  ;; (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer))

(use-package corfu-doc
  :after corfu
  :custom
  (corfu-doc-auto nil)
  :hook
  (corfu-mode . corfu-doc-mode)
  :bind
  ("C-c c d" . corfu-doc-toggle))

(use-package kind-icon
  :after corfu
  ;;:commands kind-icon-enhance-completion kind-icon-margin-formatter
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (add-hook 'my-completion-ui-mode-hook
   	    (lambda ()
   	      (setq completion-in-region-function
   		    (kind-icon-enhance-completion
   		     completion-in-region-function)))))

;;; 10-environment.el ends here
;;; End:
