;;; init.el --- Emacs init
;;; Commentary:

;;; Code:

(setenv "XAPIAN_CJK_NGRAM" "1")

(setq user-full-name "Zhou Peng")
(setq package-archives '(("gnu-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa-cn" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
;; Initialize packages
(when (< emacs-major-version 27)
  (package-initialize))

;; add site-lisp to load path
(add-to-list 'load-path (concat user-emacs-directory "environment"))
;;(add-to-list 'load-path (concat user-emacs-directory "site-lisp/mu4e"))

;; add site-themes to theme load path
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes"))

;;(customize-set-variable 'custom-file (concat user-emacs-directory "custom-local.el"))
;;(load custom-file 'noerror)

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
