;;; init.el --- Emacs init
;;; Commentary:

;;; Code:

(setenv "XAPIAN_CJK_NGRAM" "1")

;; set up package repositories.
(require 'package)
(setq package-archives '(("gnu-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa-cn" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("zelpa" . "~/.emacs.d/zelpa")))
;; Initialize packages
(package-initialize)

;; add site-lisp to load path
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp/mu4e")

;; add site-themes to theme load path
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/site-themes"))

(customize-set-variable 'custom-file "~/.emacs.d/custom-local.el")
(load custom-file 'noerror)

(require 'use-package)

(load "ldap-mode")

(use-package base-env)

(use-package org-env)

(use-package mail-env
    :config
  ;; key bindings
  ;; Bind key <F12> to open 'mu4e'
  (global-set-key (quote [f12]) (quote mu4e)))

(use-package news-env)

(use-package prog-env)

;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "C-c r f") 'rtags-find-symbol)
;; (global-set-key (kbd "C-c r c") 'rtags-find-symbol-at-point)
;; (global-set-key (kbd "C-c r p") 'rtags-print-symbol-info)
;; (global-set-key (kbd "C-c r b") 'rtags-location-stack-back)

(use-package pui
  ;; :hook
  ;; ((org-mode . set-org-buffer-variable-pitch)
  ;;  (markdown-mode . set-markdown-buffer-variable-pitch))
  :config
  (use-package eshell
    :bind ("C-c s" . eshell))
  ;; binding key control-\ to comment/uncomment.
  (global-set-key (kbd "C-c l") 'pui-comment-or-uncomment-region-or-line)
  ;; binding key control-c control-e to beautify.
  (global-set-key (kbd "C-c b f") 'pui-beautify))

;; load spacemacs dark theme
(load-theme 'spacemacs-dark t)

(server-start)

;;(load "site-start")

(provide 'init)
;;; init.el ends here
