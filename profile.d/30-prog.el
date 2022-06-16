;;; 30-prog.el --- Site start
;;; Commentary:

;;; Code:

(defvar home-directory)
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

(use-package rainbow-delimiters
  :demand
  :hook (prog-mode . rainbow-delimiters-mode))

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

(use-package typescript-mode
  :init
  (define-derived-mode typescript-tsx-mode typescript-mode "TypeScript")
  :config
  (add-to-list 'auto-mode-alist '("\\.[jt]sx?\\'" . typescript-tsx-mode)))

(use-package prettier
  :hook
  ((js-mode . prettier-mode)
   (typescript-mode . prettier-mode)))

(use-package tree-sitter
  :hook
  ((go-mode . tree-sitter-hl-mode)
   (typescript-mode . tree-sitter-hl-mode)
   (typescript-tsx-mode . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :after tree-sitter
  :config
  (tree-sitter-require 'tsx)
  (tree-sitter-require 'go)
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx)))

(use-package docker-compose-mode)

(use-package lsp-mode
  :commands lsp-register-client make-lsp-client lsp-stdio-connection lsp-yaml-server-command lsp-activate-on executable-find lsp-package-path
  :custom
  (lsp-keymap-prefix "C-c C-l")
  (lsp-fsharp-server-install-dir (concat emacs-data-dir "lsp/fsautocomplete"))
  (lsp-java-server-install-dir (concat emacs-data-dir "lsp/eclipse.jdt.ls"))
  (lsp-xml-server-work-dir (concat emacs-data-dir "lsp/xml"))
  (lsp-server-install-dir (concat emacs-data-dir "lsp"))
  (lsp-session-file (concat emacs-data-dir "lsp/lsp-session-v1"))
  (lsp-clients-typescript-plugins
   (vector (list :name "@vsintellicode/typescript-intellicode-plugin"
                 :location (concat home-directory ".vscode/extensions/visualstudioexptteam.vscodeintellicode-1.2.14"))
           (list :name "vscode-chrome-debug-core"
                 :location (concat home-directory ".vscode/extensions/msjsdiag.debugger-for-chrome-4.13.0"))))
  ;;(lsp-clients-typescript-server-args '("--stdio" "--tsserver-log-file" "/tmp/tss.log" "--log-level" "log" "--tsserver-log-verbosity" "verbose"))
  ;;(lsp-clients-typescript-tls-path "/usr/local/bin/tsserver")
  ;;(lsp-clients-typescript-server-args "--server")
  (lsp-enable-snippet nil)
  ;;(lsp-enable-symbol-highlighting nil)
  (lsp-log-io nil)
  ;;(lsp-headerline-breadcrumb-mode nil)
  (gc-cons-threshold (* 64 1024 1024))
  (read-process-output-max (* 2 1024 1024))
  :hook
  ((c-mode . lsp)
   (c++-mode . lsp)
   (java-mode . lsp)
   (python-mode . lsp)
   (go-mode . lsp)
   (json-mode . lsp)
   (yaml-mode . lsp)
   (shell-script-mode . lsp)
   (typescript-mode . lsp)
   (docker-compose-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :init
  (use-package lsp-yaml
    :config
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection
                       (lambda ()
                         `(,(or (executable-find (cl-first lsp-yaml-server-command))
                                (lsp-package-path 'yaml-language-server))
                           ,@(cl-rest lsp-yaml-server-command))))
      :major-modes '(docker-compose-mode)
      :server-id 'docker-compose-yamlls
      :activation-fn (lsp-activate-on "docker-compose"))))
  :config
  (add-to-list 'lsp-language-id-configuration
               '(docker-compose-mode . "docker-compose"))
  (use-package lsp-diagnostics
    :custom
    (lsp-diagnostics-provider :auto)))

;; (use-package lsp-ui
;;   :commands lsp-ui-mode
;;   :custom
;;   (lsp-ui-sideline-show-diagnosqtics nil)
;;   (lsp-ui-sideline-show-hover nil)
;;   (lsp-ui-sideline-show-code-actions nil)
;;   (lsp-ui-sideline-update-mode nil)
;;   (lsp-ui-doc-enable nil)
;;   (lsp-ui-peek-enable nil)
;;   (lsp-ui-peek-show-directory nil)
;;   (lsp-ui-sideline-enable nil))

(defconst lombok-path
  (expand-file-name (file-name-as-directory "~/.local/lib/lombok")))

(use-package lsp-java
  :defines lsp-java-vmargs
  :init
  (setenv "M2_REPO" (expand-file-name (file-name-as-directory "~/.var/lib/m2")))
  :config
  (add-to-list 'lsp-java-vmargs
               (concat "-javaagent:" lombok-path "lombok.jar"))
  (add-to-list 'lsp-java-vmargs
               (concat "-Xbootclasspath/a:" lombok-path "lombok.jar"))
  :custom
  (lsp-java-imports-gradle-wrapper-checksums [(
                                               :sha256 "e996d452d2645e70c01c11143ca2d3742734a28da2bf61f25c82bdc288c9e637"
                                               :allowed t)])
  (lsp-java-workspace-dir (concat emacs-data-dir "workspace/"))
  (lsp-java-workspace-cache-dir (concat emacs-data-dir "workspace/.cache/"))
  ;;(lsp-java-jdt-download-url "https://mirrors.ustc.edu.cn/eclipse/jdtls/snapshots/jdt-language-server-latest.tar.gz")
  ;;(lsp-java-jdt-download-url "https://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz")
  (lsp-java-jdt-download-url "https://nexus.nroad.com.cn/repository/maven-releases/org/eclipse/jdt/ls/language-server/latest/language-server-latest.tar.gz")
  (lsp-java-configuration-maven-user-settings
   (expand-file-name "~/.var/lib/m2/settings.xml")))
  ;; (lsp-java-vmargs (list
  ;;                   "-noverify"
  ;;                   "-Xmx2G"
  ;;                   "-XX:+UseG1GC"
  ;;                   "-XX:+UseStringDeduplication"
  ;;                   (concat "-javaagent:" lombok-path "lombok.jar")
  ;;                   (concat "-Xbootclasspath/a:" lombok-path "lombok.jar")
  ;;                   "--add-modules=ALL-SYSTEM"
  ;;                   "--add-opens"
  ;;                   "java.base/java.util=ALL-UNNAMED"
  ;;                   "--add-opens"
  ;;                   "java.base/java.lang=ALL-UNNAMED")))

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
  (use-package dap-dlv-go :demand)
  (use-package dap-ui
    :bind (("C-c d m" . dap-ui-show-many-windows)
           ("C-c d n" . dap-ui-hide-many-windows))))

(use-package which-key
  :config
  (which-key-mode))

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
