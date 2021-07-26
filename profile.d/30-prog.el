;;; 30-prog.el --- Site start
;;; Commentary:

;;; Code:

(defvar emacs-data-dir)

(use-package magit
  :bind ("C-c m g" . magit-status-here))

(use-package prog-mode
  :defines c-basic-offset
  :custom
  (c-basic-offset 4)
   ;; language-environtment "UTF-8"
   ;; speedbar-mode-hook (quote variable-pitch-mode)
  (tab-stop-list (number-sequence 4 120 4)))

(use-package display-line-numbers
  :demand
  :hook (prog-mode . display-line-numbers-mode))

(use-package go-mode)
(use-package go-eldoc)

(use-package python
  :interpreter ("python3" . python-mode)
  :custom
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "-i"))

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

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.[tj]sx?\\'" . web-mode)))

(use-package typescript-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.[jt]sx?\\'" . typescript-mode)))

(use-package prettier
  :hook
  ((js-mode . prettier-mode)
   (typescript-mode . prettier-mode)))

(use-package lsp-mode
  :custom
  (lsp-keymap-prefix "C-c C-l")
  (lsp-fsharp-server-install-dir (concat emacs-data-dir "lsp/fsautocomplete"))
  (lsp-java-server-install-dir (concat emacs-data-dir "lsp/eclipse.jdt.ls"))
  (lsp-xml-server-work-dir (concat emacs-data-dir "lsp/xml"))
  (lsp-server-install-dir (concat emacs-data-dir "lsp"))
  (lsp-session-file (concat emacs-data-dir "lsp/lsp-session-v1"))
  (lsp-clients-typescript-plugins
   (vector (list :name "@vsintellicode/typescript-intellicode-plugin"
                 :location "/home/pengz/.vscode/extensions/visualstudioexptteam.vscodeintellicode-1.2.14")
           (list :name "vscode-chrome-debug-core"
                 :location "/home/pengz/.vscode/extensions/msjsdiag.debugger-for-chrome-4.12.12")))
  ;;(lsp-clients-typescript-server-args '("--stdio" "--tsserver-log-file" "/tmp/tss.log" "--log-level" "log" "--tsserver-log-verbosity" "verbose"))
  ;;(lsp-clients-typescript-tls-path "/usr/local/bin/tsserver")
  ;;(lsp-clients-typescript-server-args "--server")
  :hook
  ((c-mode . lsp)
   (java-mode . lsp)
   (python-mode . lsp)
   (go-mode . lsp)
   (json-mode . lsp)
   (yaml-mode . lsp)
   (shell-script-mode . lsp)
   (typescript-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :bind ("C-c h a" . helm-lsp-code-actions)
  :config
  (use-package lsp-diagnostics
    :custom
    (lsp-diagnostics-provider :auto))
  :custom
  (gc-cons-threshold 100000000)
  (read-process-output-max (* 1024 1024)))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-sideline-enable nil))

(defconst lombok-path
  (expand-file-name (file-name-as-directory "~/.local/lib/lombok")))

(use-package lsp-java
  :init
  (setenv "M2_REPO" (expand-file-name (file-name-as-directory "~/.var/lib/m2")))
  :custom
  (lsp-java-imports-gradle-wrapper-checksums [(
   :sha256 "e996d452d2645e70c01c11143ca2d3742734a28da2bf61f25c82bdc288c9e637"
   :allowed t)])
  (lsp-java-workspace-dir (concat emacs-data-dir "workspace/"))
  (lsp-java-workspace-cache-dir (concat emacs-data-dir "workspace/.cache/"))
  (lsp-java-jdt-download-url "https://mirrors.ustc.edu.cn/eclipse/jdtls/snapshots/jdt-language-server-latest.tar.gz")
  (lsp-java-configuration-maven-user-settings
   (expand-file-name "~/.var/lib/m2/settings.xml"))
  (lsp-java-vmargs (list
                    "-noverify"
                    "-Xmx2G"
                    "-XX:+UseG1GC"
                    "-XX:+UseStringDeduplication"
                    (concat "-javaagent:" lombok-path "lombok.jar")
                    (concat "-Xbootclasspath/a:" lombok-path "lombok.jar")
                    "--add-modules=ALL-SYSTEM"
                    "--add-opens"
                    "java.base/java.util=ALL-UNNAMED"
                    "--add-opens"
                    "java.base/java.lang=ALL-UNNAMED")))

(use-package lsp-sonarlint
  :demand
  :config
  (use-package lsp-sonarlint-java
    :custom
    (lsp-sonarlint-java-enabled t)))

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
  (use-package dap-python :demand
    :custom
    (dap-python-terminal "gnome-terminal -- ")
    (dap-python-executable "python3"))
  (use-package dap-chrome :demand)
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
