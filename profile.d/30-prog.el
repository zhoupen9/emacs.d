;;; -*- lexical-binding: t; -*-
;;; 30-prog.el --- Site start
;;; Commentary:

;;; Code:

(defvar home-directory)
(defvar emacs-data-dir)

(use-package magit
  :config
  (add-hook 'magit-status-sections-hook #'magit-insert-worktrees t)
  :bind ("C-c g" . magit-status-here))

(use-package magit-delta
  :hook (magit-mode . magit-delta-mode))

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

(use-package eldoc
  :config
  (global-eldoc-mode -1))

(use-package eglot
  :config
  (setq-default eglot-workspace-configuration
              '(:typescript (:formatOptions (:enable nil)
                             :documentFormattingProvider :json-false
                             :documentRangeFormattingProvider :json-false)
                :javascript (:formatOptions (:enable nil)
                             :documentFormattingProvider :json-false
                             :documentRangeFormattingProvider :json-false)))
  :init
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 `((java-mode java-ts-mode) .
                   ("jdtls"
                    :initializationOptions
                    (:bundles [(expand-file-name "~/.vscode/extensions/vscjava.vscode-java-debug-0.55.0/server/com.microsoft.java.debug.plugin-0.50.0.jar")]))))))

(use-package xref
  :bind
  (("C-c M-[" . xref-find-definitions)
   ("C-c M-]" . xref-go-back)))

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
  ;;(global-flycheck-mode t)
  :hook
  ;;((go-mode . flycheck-mode))
  (python-mode . flycheck-mode))

(use-package projectile
  :demand
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom
  (projectile-known-projects-file (concat emacs-data-dir "projectile-bookmarks.eld"))
  (projectile-cache-file (concat emacs-data-dir "projectile.cache"))
  (projectile-git-use-fd nil)
  (projectile-git-submodule-command nil)
  :config
  (use-package consult-projectile
    :bind
    ("C-c f" . consult-projectile-find-file)
    ("C-c e" . consult-projectile-recentf)
    ("C-c o" . consult-projectile-switch-project))
  (projectile-mode t))

(use-package treemacs-customization
  :custom
  (treemacs-persist-file (concat emacs-data-dir "treemacs-persist")))

(use-package go-ts-mode
  :config
  (use-package cc-vars)
  :custom
  (go-ts-mode-indent-offset 4))
;;  :config
;;  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode)))

(use-package typescript-ts-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode)))

(use-package prettier
  :hook
  ((js-mode . prettier-mode)
   (typescript-ts-mode . prettier-mode)
   (tsx-ts-mode . prettier-mode)))

(use-package treesit
  :custom
  (treesit-auto-install-grammer t)
  (treesit-enabled-modes t)
  (major-mode-remap-alist
   '((c-mode . c-ts-mode)
     (c++-mode . c++-ts-mode)
     (cmake-mode . cmake-ts-mode)
     (dockerfile-mode . dockerfile-ts-mode)
     (go-mode . go-ts-mode)
     (python-mode . python-ts-mode)
     (json-mode . json-ts-mode)
     (java-mode . java-ts-mode)
     (rust-mode . rust-ts-mode)
     (ruby-mode . ruby-ts-mode)
     (typescript-ts-mode . typescript-ts-mode)
     (conf-toml-mode . toml-ts-mode)
     (yaml-mode . yaml-ts-mode))))

;; (use-package docker-compose-mode)

(defconst lombok-path
  (expand-file-name (file-name-as-directory "~/.local/lib/lombok")))

(use-package which-key
  :config
  (which-key-mode))

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

(use-package symbol-overlay
  :bind
  (("C-c n i" . symbol-overlay-put)
   ("C-c n f" . symbol-overlay-switch-forward)
   ("C-c n p" . symbol-overlay-switch-backward)
   ("C-c n n" . symbol-overlay-remove-all)
   ("C-c n >" . symbol-overlay-jump-next)
   ("C-c n <" . symbol-overlay-jump-prev)))

(defun ansi-color-apply-compilation-buffer ()
  "Apply ansi color for compilation buffer."
  (read-only-mode nil)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (read-only-mode))

(add-hook 'compilation-filter-hook 'ansi-color-apply-compilation-buffer)

(use-package dape
  ;; To use window configuration like gud (gdb-mi)
  ;; :init
  ;; (setq dape-buffer-window-arrangement 'gud)
  :custom
  (dape-adapter-dir (concat emacs-data-dir "debug-adapters"))
  :config
  ;; Info buffers to the right
  ;; (setq dape-buffer-window-arrangement 'right)

  ;; To not display info and/or buffers on startup
  ;; (remove-hook 'dape-on-start-hooks 'dape-info)
  ;; (remove-hook 'dape-on-start-hooks 'dape-repl)

  ;; To display info and/or repl buffers on stopped
  ;; (add-hook 'dape-on-stopped-hooks 'dape-info)
  ;; (add-hook 'dape-on-stopped-hooks 'dape-repl)

  ;; By default dape uses gdb keybinding prefix
  ;; (setq dape-key-prefix "\C-x\C-a")

  ;; Kill compile buffer on build success
  ;; (add-hook 'dape-compile-compile-hooks 'kill-buffer)

  ;; Save buffers on startup, useful for interpreted languages
  ;; (add-hook 'dape-on-start-hooks
  ;;           (defun dape--save-on-start ()
  ;;             (save-some-buffers t t)))

  ;; Projectile users
  (setq dape-cwd-fn (lambda (&optional skip-tramp-trim)
                      (let ((root (projectile-project-root)))
                        (if (and (not skip-tramp-trim) (tramp-tramp-file-p root))
                            (tramp-file-name-localname (tramp-dissect-file-name root))
                          root))))
  )

(add-to-list 'dape-configs
             `(delve-unit-test
               modes (go-mode go-ts-mode)
               ensure dape-ensure-command
               fn dape-config-autoport
               command "dlv"
               command-args ("dap" "--listen" "127.0.0.1::autoport")
               command-cwd dape-cwd-fn
               port :autoport
               :type "debug"
               :request "launch"
               :mode (lambda () (if (string-suffix-p "_test.go"   (buffer-name)) "test" "debug"))
               :cwd dape-cwd-fn
               :program (lambda () (if (string-suffix-p "_test.go"   (buffer-name))
                                       (concat "./" (file-relative-name default-directory (funcall dape-cwd-fn)))
                                     (funcall dape-cwd-fn)))
               :args (lambda ()
                       (require 'which-func)
                       (if (string-suffix-p "_test.go"   (buffer-name))
                           (when-let* ((test-name (which-function))
                                       (test-regexp (concat "^" test-name "$")))
                             (if test-name `["-test.run" ,test-regexp]
                               (error "No test selected")))
                         []))))

;;; 30-prog.el ends here
;;; End:
