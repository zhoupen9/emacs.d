;;; prog-env.el --- Site start
;;; Commentary:

;;; Code:


(use-package prog-mode
  :defines c-basic-offset
  :config
  (setq
   c-basic-offset 4
   ;; language-environtment "UTF-8"
   ;; speedbar-mode-hook (quote variable-pitch-mode)
   tab-stop-list (number-sequence 4 120 4))
  :hook
  (
   ;; Setup virtual studio programming mode hooks.
   ;; display line number in programing modes
   (prog-mode-hook . display-line-numbers-mode)
   (prog-mode-hook . company-mode)))
   ;;(add-hook 'prog-mode-hook 'company-quickhelp-mode)

(use-package go-mode)
(use-package go-eldoc)

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python3" . python-mode))

;; flycheck
(use-package flycheck
  :config
  (setq flycheck-emacs-lisp-load-path (quote inherit))
  (use-package flycheck-pyflakes)
  (global-flycheck-mode t)
  :hook
  ((go-mode-hook . flycheck-mode))
  (python-mode-hook . flycheck-mode))

(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode t))

(use-package helm
  :config
  (helm-mode t)
  (helm-projectile-on)
  :bind (("M-x" . helm-M-x)))

(use-package lsp-mode
  :config
  (setq lsp-keymap-prefix "C-c C-l")
  :hook
  ((c-mode . lsp)
   (java-mode . lsp)
   (python-mode . lsp)
   (go-mode . lsp))
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package helm-lsp
  :commands helm-lsp-workspace-symbol)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

(use-package dap-mode)

(use-package which-key
    :config
    (which-key-mode))

(use-package company
  :bind (("C-c c c" . company-complete)))

(use-package jedi-core
  :config
  (setq
   jedi:complete-on-dot t
   jedi:get-in-function-call-delay 0
   jedi:tooltip-method nil
   python-shell-interpreter "python3"))

;; language-environtment "UTF-8"
;; speedbar-mode-hook (quote variable-pitch-mode)
;; (helm-mode t)
;; (helm-projectile-on)
;; Setup virtual studio programming environtment.
;;(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; (projectile-mode t)

(use-package eldoc
  :hook
  ((emacs-lisp-mode-hook . eldoc-mode)))


;;(add-to-list 'company-backends 'company-rtags)
;;(global-set-key (kbd "C-c c c") 'company-complete)


(use-package c-eldoc
  :hook
  ((c-mode-hook . c-turn-on-eldoc-mode)))

(use-package go-eldoc
  :hook
  ((go-mode-hook . go-eldoc-setup)))

(use-package flycheck-golangci-lint
  :hook
  ((flycheck-mode-hook . flycheck-golangci-lint-setup)))


(use-package company-go
  :hook ((go-mode-hook .
          (lambda()
            (add-to-list 'company-backends 'company-go)))))

(use-package company-jedi
  :hook ((python-mode-hook .
          (lambda ()
            (add-to-list 'company-backends 'company-jedi)))))

;; (put 'flycheck-clang-args 'safe-local-variable-values (lambda(xx) t))

(use-package flycheck-yamllint
  :hook
  ((flycheck-mode-hook . flycheck-yamllint-setup)))

(use-package yasnippet
  ;;:config
  ;; (yas-reload-all)
  :hook ((prog-mode-hook . yas-minor-mode)))

(provide 'prog-env)
;;; prog-env.el ends here
;;; End:
