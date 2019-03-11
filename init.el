;;; init.el --- Emacs init
;;; Commentary:

;;; Code:

;; set up package repositories.
(require 'package)
(setq package-archives '(("gnu-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa-cn" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")
                         ("zelpa" . "~/.emacs.d/zelpa")))
;; Initialize packages
(package-initialize)

;; add site-lisp to load path
(add-to-list 'load-path "~/.emacs.d/site-lisp")

;; add site-themes to theme load path
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/site-themes"))

(customize-set-variable 'custom-file "~/.emacs.d/custom-local.el")
(load "~/.emacs.d/custom.el" 'noerror)
(load custom-file 'noerror)

(load "site-start")

(provide 'init)
;;; init.el ends here
