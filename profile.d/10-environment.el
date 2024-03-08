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
(setq desktop-path (list (concat emacs-data-dir "desktop")))
;;(setq tab-stop-list (number-sequence 4 120 4))
(add-to-list 'treesit-extra-load-path (concat emacs-data-dir "treesit/"))

;; disable reordering for languages such as aribic.
(setq-default bidi-display-reordering nil)
(setq bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)

(when (not (display-graphic-p))
  (menu-bar-mode 0))

(when (display-graphic-p)
  ;; Turn off toolbar.
  (tool-bar-mode 0)
  ;; Turn off menubar.
  (menu-bar-mode 0)
  ;; Turn off scroll bar.
  (scroll-bar-mode 0)
  ;; Turn on speed bar.
  ;; (speedbar-frame-mode t)
  (pixel-scroll-precision-mode)

  (when (find-font (font-spec :name "Literation Mono Nerd Font"))
    (set-frame-font "Literation Mono Nerd Font" nil t))
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
                        (font-spec :family "Noto Sans CJK SC")))))  ;; 中文宽度
                        ;;(font-spec :family "Noto Sans CJK SC")))) ;; 12345678

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
  :demand
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

(use-package consult
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")
  :bind
  (("C-c h" . consult-history)
   ("C-c m" . consult-mode-command)
   ("C-x b" . consult-buffer)
   ("C-x r b" . consult-bookmark)
   ("C-x p b" . consult-project-buffer)
   ("M-g g" . consult-goto-line)             ;; orig. goto-line
   ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
   ("M-g o" . consult-outline)
   ("C-c c s" . consult-find)
   ("C-c l" . consult-line)
   ("C-c r g" . consult-ripgrep)))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package rg
  :config
  (rg-enable-default-bindings))

(use-package rime
  :bind
  (:map rime-mode-map
        ("C-`" . 'rime-send-keybinding))
  :custom
  (rime-librime-root (expand-file-name "~/.local/lib"))
  (rime-share-data-dir (expand-file-name "~/.local/share/rime"))
  (rime-user-data-dir (expand-file-name "~/.config/rime"))
  (rime-emacs-module-header-root (expand-file-name "~/.local/include"))
  (default-input-method "rime")
  (rime-show-candidate 'posframe))

;;; 10-environment.el ends here
;;; End:
