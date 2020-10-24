;;; 30-prog.el --- Site start
;;; Commentary:

;;; Code:

(defvar emacs-data-dir)

(use-package magit
  :bind ("C-c m g" . magit-status-here))

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
  :interpreter ("python3" . python-mode)
  :config
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i"))

;; flycheck
(use-package flycheck
  :demand
  :config
  (setq flycheck-emacs-lisp-load-path (quote inherit))
  (global-flycheck-mode t)
  :hook
  ((go-mode . flycheck-mode))
  (python-mode . flycheck-mode))

(use-package projectile
  :demand
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom
  (projectile-known-projects-file (concat emacs-data-dir "projectile-bookmarks.eld"))
  (projectile-cache-file (concat emacs-data-dir "projectile.cache"))
  :config
  (projectile-mode t))

(use-package treemacs-customization
  :custom
  (treemacs-persist-file (concat emacs-data-dir "treemacs-persist")))

(use-package lsp-mode
  :custom
  (lsp-keymap-prefix "C-c C-l")
  (lsp-fsharp-server-install-dir (concat emacs-data-dir "lsp/fsautocomplete/"))
  (lsp-java-server-install-dir (concat emacs-data-dir "lsp/eclipse.jdt.ls/"))
  (lsp-xml-server-work-dir (concat emacs-data-dir "lsp/xml/"))
  (lsp-server-install-dir (concat emacs-data-dir "lsp/"))
  (lsp-session-file (concat emacs-data-dir "lsp/lsp-session-v1"))
  :hook
  ((c-mode . lsp)
   (java-mode . lsp)
   (python-mode . lsp)
   (go-mode . lsp)
   (json-mode . lsp)
   (yaml-mode . lsp)
   (shell-script-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :bind ("C-c h a" . helm-lsp-code-actions)
  :config
  (use-package lsp-diagnostics
    :custom
    (lsp-diagnostics-provider :auto))
  (setq gc-cons-threshold 100000000
        read-process-output-max (* 1024 1024)))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-sideline-enable nil))

(use-package lsp-java
  :custom
  (lsp-java-workspace-dir (concat emacs-data-dir "workspace/"))
  (lsp-java-workspace-cache-dir (concat emacs-data-dir "workspace/.cache/"))
  (lsp-java-vmargs (list
                    "-noverify"
                    "-Xmx2G"
                    "-XX:+UseG1GC"
                    "-XX:+UseStringDeduplication")))
;; :custom (lsp-java-server-install-dir "~/.local/lib/eclipse.jdt.ls"))

(use-package helm-lsp
  :after (helm)
  :commands helm-lsp-workspace-symbol)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

(use-package dap-mode
  :custom
  (dap-breakpoints-file (concat emacs-data-dir ".dap-breakpoints"))
  :config
  (use-package dap-utils
    :custom
    (dap-utils-extension-path (concat emacs-data-dir "lsp/extensions/")))
  (use-package dap-hydra
    :bind ("C-c d h" . dap-hydra))
  (use-package dap-lldb :demand
    :custom
    (dap-lldb-debug-program
     (concat emacs-data-dir
             "lsp/extensions/vscode/llvm-org.lldb-vscode-0.1.0/bin/lldb-vscode")))
  (use-package dap-java
    :bind
    (("C-c t m" . dap-java-debug-test-method)
     ("C-c t c" . dap-java-debug-test-class))
    :custom
    (dap-java-test-runner (concat lsp-java-server-install-dir "/test-runner/junit-platform-console-standalone.jar")))
  (use-package dap-cpptools :demand)
  (use-package dap-ui
    :bind (("C-c d m" . dap-ui-show-many-windows)
           ("C-c d n" . dap-ui-hide-many-windows))))

(use-package which-key
    :config
    (which-key-mode))

(use-package company
  :custom (company-tooltip-minimum-width 80)
  :hook
  ((prog-mode . company-mode)
   (json-mode . company-mode)
   (yaml-mode . company-mode)
   (xml-mode . company-mode)
   (js-mode . company-mode))
  :bind (("C-c c c" . company-complete)))

(use-package company-box
  :custom (company-box-tooltip-maximum-width 520)
  :hook (company-mode . company-box-mode))

(use-package company-go
  :hook ((go-mode .
          (lambda()
            (add-to-list 'company-backends 'company-go)))))

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
  :hook ((prog-mode . yas-minor-mode)))

(use-package nxml-mode
  :custom
  (nxml-slash-auto-complete-flag t))

(require 'ansi-color)
(require 'files)
(require 'compile)

(defun ansi-color-apply-compilation-buffer ()
  "Apply ansi color for compilation buffer."
  (read-only-mode nil)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (read-only-mode))

(add-hook 'compilation-filter-hook 'ansi-color-apply-compilation-buffer)

;;; 30-prog.el ends here
;;; End:
