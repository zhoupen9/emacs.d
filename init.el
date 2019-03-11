;;; init.el --- Emacs init file
;;; Commentary:
;;; Emacs init script

;;; Code:

;;; set up package repositories.
(require 'package)
(setq package-archives '(("gnu-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa-cn" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("zelpa" . "~/.emacs.d/zelpa")))
(package-initialize)

;; add site-lisp to load path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp"))

;; add site-themes to theme load path
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/site-themes"))

(require 'ldap-mode)

(require 'virtualstudio)
(virtualstudio-initialize)

;;(server-start)

(provide 'init);
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yasnippet yaml-mode virtualstudio use-package spacemacs-theme scala-mode python-mode pyim posframe org markdown-mode magit helm-rtags helm-projectile helm-gtags helm-go-package helm-git helm-flycheck helm-company groovy-mode gradle-mode go-eldoc ggtags flycheck-yamllint flycheck-pyflakes flycheck-gradle flycheck-golangci-lint exec-path-from-shell dockerfile-mode company-rtags company-quickhelp company-jedi company-go company-flx cmake-mode clang-format c-eldoc))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
