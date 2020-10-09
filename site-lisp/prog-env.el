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
   tab-stop-list (number-sequence 4 120 4)))

(use-package display-line-numbers
  :demand
  :hook (prog-mode . display-line-numbers-mode))

(use-package go-mode)
(use-package go-eldoc)

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python3" . python-mode))

;; flycheck
(use-package flycheck
  :demand
  :config
  (setq flycheck-emacs-lisp-load-path (quote inherit))
  (use-package flycheck-pyflakes)
  (global-flycheck-mode t)
  :hook
  ((go-mode . flycheck-mode))
  (python-mode . flycheck-mode))

(use-package projectile
  :demand
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode t))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c C-l")
  :hook
  ((c-mode . lsp)
   (java-mode . lsp)
   (python-mode . lsp)
   (go-mode . lsp)
   (json-mode . lsp)
   (yaml-mode . lsp)
   (shell-script-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package lsp-java)
;;  :custom (lsp-java-server-install-dir "~/.local/lib/eclipse.jdt.ls"))

(use-package helm-lsp
  :after (helm)
  :commands helm-lsp-workspace-symbol)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

(use-package dap-mode
  :bind ("C-c d" . dap-debug)
  :config
  (use-package dap-gdb-lldb)
  (use-package dap-cpptools :demand))

(use-package which-key
    :config
    (which-key-mode))

(use-package company
  :custom (company-tooltip-minimum-width 80)
  :hook (prog-mode . company-mode)
  :bind (("C-c c c" . company-complete)))

(use-package company-box
  :custom (company-box-tooltip-maximum-width 520)
  :hook (company-mode . company-box-mode))

(use-package company-quickhelp
  :hook (company-mode . company-quickhelp-mode))

(use-package company-go
  :hook ((go-mode .
          (lambda()
            (add-to-list 'company-backends 'company-go)))))

(use-package company-jedi
  :hook ((python-mode .
          (lambda ()
            (add-to-list 'company-backends 'company-jedi)))))

(use-package jedi-core
  :config
  (setq
   jedi:complete-on-dot t
   jedi:get-in-function-call-delay 0
   jedi:tooltip-method nil
   python-shell-interpreter "python3"))

(use-package eldoc
  :hook
  ((emacs-lisp-mode . eldoc-mode)))

(use-package c-eldoc
  :hook
  ((c-mode . c-turn-on-eldoc-mode)))

(use-package go-eldoc
  :hook
  ((go-mode . go-eldoc-setup)))

(use-package flycheck-golangci-lint
  :hook
  ((flycheck-mode . flycheck-golangci-lint-setup)))


(use-package flycheck-yamllint
  :hook
  ((flycheck-mode . flycheck-yamllint-setup)))

(use-package yasnippet
  ;;:config
  ;; (yas-reload-all)
  :hook ((prog-mode . yas-minor-mode)))

(provide 'prog-env)
;;; prog-env.el ends here
;;; End:
