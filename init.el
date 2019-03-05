;;; init.el --- Emacs init file
;;; Commentary:

;;; Code:
;;; set up package repositories.
(require 'package)

(setq package-archives '(("gnu-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa-cn" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

;; set up load-path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp"))
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/site-themes"))

;; UTF-8 as default encoding
(set-language-environment "UTF-8")

;; change title bar display
(setq frame-title-format '("" "[%b] %f - Emacs " emacs-version))

(require 'speedbar)
(add-to-list 'speedbar-mode-hook 'variable-pitch-mode)

(when (display-graphic-p)
  (tool-bar-mode 0)  ;; Turn off toolbar.
  ;; (menu-bar-mode 0)  ;; Turn off menubar
  (speedbar-frame-mode)
  ;; (fringe-mode 0)  ;; Turn off fringle mode
  ;; (set-scroll-bar-mode 'right)  ;; right side scroll bar
  (scroll-bar-mode 0)
  
  ;; Chinese Font
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
			  charset
			  (font-spec :family "Noto Sans CJK SC"))))

;; Set font color
(global-font-lock-mode t)

;; Hilight selection
;; (pc-selection-mode)

;; time format
(display-time)
;; (setq display-time-24hr-format t)
(setq display-time-day-and-date t)

;; set 'yes or no' to 'y or n'
(fset 'yes-or-no-p 'y-or-n-p)

;; highlight marked block
(setq transient-mark-mode t)

;; display line and column number
(setq column-number-mode t)
(setq line-number-mode t)

;; show paren
(show-paren-mode t)

;; shutdown bell
(setq visible-bell t)

;; set tab stop to 4.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq tab-stop-list (number-sequence 4 120 4))

;; set backup dir
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

;; Enable electric minor mode
(electric-indent-mode 1)

;; 去掉 Emacs 和 gnus 启动时的引导界面
(setq inhibit-startup-message t)

;; Add confirmation for quit emacs
(setq confirm-kill-emacs 'yes-or-no-p)

(require 'face-remap)
(add-hook 'markdown-mode-hook 'variable-pitch-mode)

;; ;; setup yasnippet
;; (setq yas-snippet-dirs `("~/.emacs.d/snippets"))
;; (eval-after-load "autopair-autoloads"
;;   '(progn
;; 	 (require 'yasnippet)
;; 	 (yas-global-mode 1)
;;      (define-key yas-minor-mode-map (kbd "<tab>") nil)
;;      (define-key yas-minor-mode-map (kbd "TAB") nil)
;;      (define-key yas-minor-mode-map (kbd "C-c C-o") 'yas-expand)))

;; (add-hook 'prog-mode-hook 'yas-minor-mode)

(require 'helm-config)
(require 'rtags)
(setq rtags-completions-enabled t)
(setq rtags-display-result-backend 'helm)

(require 'helm)
(helm-mode t)

(require 'helm-projectile)
(helm-projectile-on)

(require 'helm-command)
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t)

;; display line number in programing modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'company-mode)
(add-hook 'prog-mode-hook 'company-quickhelp-mode)

(require 'projectile)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode t)

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(require 'company)
(require 'company-rtags)
(add-to-list 'company-backends 'company-rtags)
(global-set-key (kbd "C-c c c") 'company-complete)

(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(require 'flycheck)
(setq flycheck-emacs-lisp-load-path 'inherit)
(global-flycheck-mode)

(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

(require 'go-mode)
(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'flycheck-mode-hook 'flycheck-golangci-lint-setup)
(require 'go-eldoc)
(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-hook 'go-mode-hook
          (lambda()
            (add-to-list 'company-backends 'company-go)))

;; (require 'python-mode)
(require 'flycheck-pyflakes)
(require 'jedi-core)
(add-hook 'python-mode-hook
          (lambda ()
            (add-to-list 'company-backends 'company-jedi)
            (setq jedi:complete-on-dot t)
            (setq jedi:tooltip-method nil)
            (setq jedi:get-in-function-call-delay 0)))
(add-hook 'python-mode-hook 'flycheck-mode)
(put 'flycheck-clang-args 'safe-local-variable-values (lambda(xx) t))

(require 'flycheck-yamllint)
(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup)

(require 'ldap-mode)

;; Load macros defined in site directory.
(require 'zoe)
(add-hook 'org-mode-hook 'set-org-buffer-variable-pitch)
(global-set-key (kbd "C-c r f") 'rtags-find-symbol)
(global-set-key (kbd "C-c r c") 'rtags-find-symbol-at-point)
(global-set-key (kbd "C-c r p") 'rtags-print-symbol-info)
(global-set-key (kbd "C-c r b") 'rtags-location-stack-back)

(require 'use-package)

(when (eq system-type 'gnu/linux)
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
      (setq pyim-page-length 5)))

(load-theme 'spacemacs-dark t)
(set-window-titlebar-theme-variant "dark")

;; ;; set up for web-beautify
;; ;; (require 'web-beautify) ;; Not necessary if using ELPA package
;; (eval-after-load 'js2-mode
;;   '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
;; (eval-after-load 'json-mode
;;   '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))
;; (eval-after-load 'sgml-mode
;;   '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))
;; (eval-after-load 'css-mode
;;   '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default)))
 '(package-selected-packages
   (quote
    (flycheck-yamllint yaml-mode c-eldoc dockerfile-mode cmake-mode helm-projectile posframe flycheck-gradle groovy-mode gradle-mode helm-rtags clang-format go-eldoc helm-company helm-flycheck helm-git helm-go-package helm-gtags company-jedi exec-path-from-shell company-quickhelp jedi-core pos-tip flycheck-pyflakes flycheck-golangci-lint flycheck company-flx flx markdown-mode spacemacs-theme pyim projectile magit ggtags company-rtags org company-go company go-mode python-mode use-package yasnippet))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:family "Liberation Mono"))))
 '(variable-pitch ((t (:height 1.11 :family "Roboto")))))

;; start emacs server
(server-start)

(provide 'init)
;;; init.el ends here
