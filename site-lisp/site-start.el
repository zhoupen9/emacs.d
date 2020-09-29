;;; site-start.el --- Site start
;;; Commentary:

;;; Code:

;;Initialize exec-path-from-shell
(require 'exec-path-from-shell)
(setq exec-path-from-shell-check-startup-files nil)
(exec-path-from-shell-initialize)

(setq
 abbrev-file-name "~/.emacs.d/.abbrev_defs"
 backup-by-copying t
 backup-by-copying-when-linked t
 backup-directory-alist (quote (("" . "~/.emacs.d/backup")))
 custom-file "~/.emacs.d/custom-local.el"
 display-time-day-and-date t
 doom-modeline-mode t)

;; 去掉 Emacs 和 gnus 启动时的引导界面
(setq inhibit-startup-message t)

(require 'mu4e)
(require 'mu4e-alert)
(require 'smtpmail)

;; "Setup mu4e.
;;(setq mu4e-use-fancy-chars t)
(setq mail-user-agent 'mu4e-user-agent)
(setq mu4e-root-maildir (expand-file-name "~/Mail"))
(setq mu4e-attachment-dir (expand-file-name "~/Mail/attachments"))
(setq user-full-name "Zhou Peng")
(setq user-mail-address "zhoupen9@sina.cn")
(setq mu4e-get-mail-command "mbsync -aqq")
(setq mu4e-update-interval 300)
(mu4e-alert-set-default-style 'libnotify)
(add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
(add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)

;; SMTP mail setting; these are the same that `gnus' use.
(setq
 message-send-mail-function   'smtpmail-send-it
 smtpmail-default-smtp-server "smtp.mxhichina.com"
 smtpmail-smtp-server         "smtp.mxhichina.com"
 smtpmail-smtp-service        465
 smtpmail-stream-type         'tls
 smtpmail-mail-address        "zhoup@nroad.com.cn"
 smtpmail-smtp-user           "zhoup@nroad.com.cn"
 smtpmail-local-domain        "mxhichina.com")

(require 'use-package)

;; Load pyim input methods if running in gnu/linux.
(use-package pyim
  :ensure nil
  :demand t
  :config
  ;; 激活 basedict 拼音词库
  (use-package pyim-basedict
    :ensure nil
    :commands pyim-basedict-enable
    :config
    (pyim-basedict-enable))
 (setq default-input-method "pyim")
  ;; 使用全拼
  (setq pyim-default-scheme 'quanpin)
  ;; ;; 开启拼音搜索功能
  ;; (pyim-isearch-mode 1)
  (setq pyim-page-tooltip 'posframe)
  ;; (setq pyim-page-tooltip 'popup)
  ;; 选词框显示5个候选词
  (setq pyim-page-length 5)
  (setq pyim-dicts
   (quote
    ((:name "bigdict" :file "~/.emacs.d/pyim/dicts/pyim-bigdict.pyim")))))

;;;###autoload
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

;;;###autoload
(defun beautify ()
  "Beautify buffer by run tabify and follow by 'indent-region."
  (interactive)
  ;; remove all trialing whitespaces.
  (whitespace-cleanup-region (point-min) (point-max))
  ;; translate spaces to tabs.
  (untabify (point-min) (point-max))
  ;; indent whole buffer.
  (indent-region (point-min) (point-max))
  ;; print message done.
  (message "Beautify buffer done."))

(require 'org)
(require 'markdown-mode)

;; Set graphic interface defaults.
;; change title bar display
(setq frame-title-format '("" "[%b] %f - Emacs " emacs-version))

;; Set font color
(global-font-lock-mode t)

;; Hilight selection
;; (pc-selection-mode)

;; time format
(display-time)
;; (setq display-time-24hr-format t)
;;(setq display-time-day-and-date t)

;; set 'yes or no' to 'y or n'
(fset 'yes-or-no-p 'y-or-n-p)

;; highlight marked block
(transient-mark-mode t)

;; display line and column number
(column-number-mode t)
(line-number-mode t)

;; show paren
(show-paren-mode t)

;; shutdown bell
(setq visible-bell t)

;; set tab stop to 4.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;;(setq tab-stop-list (number-sequence 4 120 4))

;; Enable electric minor mode
(electric-indent-mode t)

(doom-modeline-mode t)

;; Add confirmation for quit emacs
(setq confirm-kill-emacs 'yes-or-no-p)

(defun set-org-buffer-variable-pitch ()
  "Set buffer variable pitch."
  (interactive)
  (variable-pitch-mode t)
  ;; (setq line-spacing 3)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch))
;;  (set-face-attribute 'org-block-background nil :inherit 'fixed-pitch))

(defun set-markdown-buffer-variable-pitch ()
  "Set markdown buffer variable pitch."
  (interactive)
  (variable-pitch-mode t)
  (set-face-attribute 'markdown-table-face nil :inherit 'fixed-pitch))


(defun set-window-titlebar-theme-variant (variant)
  "Change emacs-gtk title bars theme VARIANT."
  (interactive "sTheme Variant Name: ")
  (let ((pid (replace-regexp-in-string  "\n$" "" (shell-command-to-string "pidof emacs"))))
    (let ((cmd (concat "wmctrl -lp | grep " pid " | awk '{ print $1 }'")))
      (let ((ids (replace-regexp-in-string "\n$" " " (shell-command-to-string cmd))))
        (dolist (id (split-string ids))
          (shell-command
           (concat "xprop -id " id " -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT " variant)))))))

(defun dark-idea ()
  "Change idea title bars theme to dark."
  (interactive)
  (let ((pid (replace-regexp-in-string  "\n$" "" (shell-command-to-string "jps | grep Main | awk '{ print $1 }'"))))
    (let ((cmd (concat "wmctrl -lp | grep " pid " | awk '{ print $1 }'")))
      (let ((ids (replace-regexp-in-string "\n$" " " (shell-command-to-string cmd))))
        (dolist (id (split-string ids))
          (shell-command
           (concat "xprop -id " id " -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT dark")))))))

(defun dark-flatpak-intellij-idea ()
  "Change idea title bars theme to dark."
  (interactive)
  (let ((cmd (concat "wmctrl -lp | grep -e \"IntelliJ IDEA$\" | awk '{ print $1 }'")))
    (let ((ids (replace-regexp-in-string "\n$" " " (shell-command-to-string cmd))))
      (dolist (id (split-string ids))
        (shell-command
         (concat "xprop -id " id " -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT dark"))))))

(defun dark-titlebar ()
  "Change title bar to dark theme."
  (interactive)
  (when (eq system-type 'darwin)
    (add-to-list 'default-frame-alist '(ns-appearance . dark)))
  (when (eq system-type 'gnu/linux)
    (set-window-titlebar-theme-variant "dark")))


(add-hook 'org-mode-hook 'set-org-buffer-variable-pitch)
(add-hook 'markdown-mode-hook 'set-markdown-buffer-variable-pitch)

(when (display-graphic-p)
  ;; Turn off toolbar.
  (tool-bar-mode 0)
  ;; Turn off menubar.
  (menu-bar-mode 0)
  ;; Turn off scroll bar.
  (scroll-bar-mode 0)
  ;; Turn on speed bar.
  (speedbar-frame-mode t)

  (when (eq system-type 'gnu/linux)
    ;; Chinese Font
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset
                        (font-spec :family "Noto Sans CJK SC")))))

(require 'company)
(require 'company-quickhelp)
(require 'company-rtags)
(require 'helm)
(require 'flycheck-pyflakes)
(require 'flycheck-yamllint)
(require 'go-mode)
(require 'go-eldoc)
(require 'jedi-core)
(require 'projectile)
(require 'python-mode)

(setq
 c-basic-offset 4
 ;; helm-M-x-fuzzy-match t
 jedi:complete-on-dot t
 jedi:get-in-function-call-delay 0
 jedi:tooltip-method nil
 ;; language-environtment "UTF-8"
 rtags-completions-enabled t
 rtags-display-result-backend (quote helm)
 speedbar-mode-hook (quote variable-pitch-mode)
 tab-stop-list (number-sequence 4 120 4)
 flycheck-emacs-lisp-load-path (quote inherit))
 
(helm-mode t)
(helm-projectile-on)

;; Setup virtual studio programming mode hooks.
;; display line number in programing modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'company-mode)
(add-hook 'prog-mode-hook 'company-quickhelp-mode)

;; Setup virtual studio programming environtment.
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(projectile-mode t)

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;;(setq python-shell-interpreter "ipython3")

(add-to-list 'company-backends 'company-rtags)
(global-set-key (kbd "C-c c c") 'company-complete)

(global-flycheck-mode t)

(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'flycheck-mode-hook 'flycheck-golangci-lint-setup)

(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-hook 'go-mode-hook
          (lambda()
            (add-to-list 'company-backends 'company-go)))

(add-hook 'python-mode-hook
          (lambda ()
            (add-to-list 'company-backends 'company-jedi)))
(add-hook 'python-mode-hook 'flycheck-mode)
(put 'flycheck-clang-args 'safe-local-variable-values (lambda(xx) t))

(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup)

;; Load elfeed-org
(require 'elfeed-org)

;; Initialize elfeed-org
;; This hooks up elfeed-org to read the configuration when elfeed
;; is started with =M-x elfeed=
(elfeed-org)

;; Optionally specify a number of files containing elfeed
;; configuration. If not set then the location below is used.
;; Note: The customize interface is also supported.
(setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org"))

;; key bindings
;; Bind key <F12> to open 'mu4e'
(global-set-key (quote [f12]) (quote mu4e))

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c r f") 'rtags-find-symbol)
(global-set-key (kbd "C-c r c") 'rtags-find-symbol-at-point)
(global-set-key (kbd "C-c r p") 'rtags-print-symbol-info)
(global-set-key (kbd "C-c r b") 'rtags-location-stack-back)
;; binding key control-\ to comment/uncomment.
(global-set-key (kbd "C-c l") 'comment-or-uncomment-region-or-line)
;; binding key control-c control-e to beautify.
(global-set-key (kbd "C-c b f") 'beautify)

;; load spacemacs dark theme
(load-theme 'spacemacs-dark t)

(server-start)

;;; site-start.el ends here
;;; End:
