;;; 30-prog.el --- Site start
;;; Commentary:

;;; Code:

(defvar home-directory)
(defvar emacs-data-dir)

(use-package magit
  :bind ("C-c g" . magit-status-here))

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

(use-package lsp-bridge-jdtls
  :hook
  (java-ts-mode
   .
   (lambda()
     (setq-local lsp-bridge-get-single-lang-server-by-project 'lsp-bridge-get-jdtls-server-by-project))))

(use-package xref
  :bind
  (("C-c M-[" . xref-find-definitions)
   ("C-c M-]" . xref-go-back)))

;; lsp-bridge
(use-package lsp-bridge
  :demand
  :commands global-lsp-bridge-mode
  :bind
  (("M-." . lsp-bridge-find-def)
   ("M-," . lsp-bridge-find-def-return)
   ("C-c M-." . lsp-bridge-find-def-other-window)
   ("C-c M-," . lsp-bridge-find-references)
   ("C-c M-p" . lsp-bridge-popup-documentation)
   ("M-[" . lsp-bridge-find-impl)
   ("M-]" . lsp-bridge-find-impl-other-window))
  :config
  (use-package acm
    :custom
    ;; (acm-enable-tabnine t)
    (acm-enable-tabnine nil)
    (acm-enable-search-file-words nil)
    (acm-enable-doc nil)
    (acm-enable-yas t)
    (acm-enable-path nil)
    (acm-enable-tempel nil))
  (global-lsp-bridge-mode)
  :hook
  ;; (java-ts-mode . lsp-bridge-mode)
  (go-ts-mode . lsp-bridge-mode)
  ;; (python-ts-mode . lsp-bridge-mode)
  ;; (lisp-interactive-mode . lsp-bridge-mode)
  ;; (emacs-lisp-mode . lsp-bridge-mode)
  :custom
  (tabnine-bridge-binaries-folder (concat emacs-data-dir "TabNine"))
  (gc-cons-threshold (* 64 1024 1024))
  (read-process-output-max (* 2 1024 1024))
  (lsp-bridge-enable-candidate-doc-preview nil)
  (lsp-bridge-enable-search-words nil)
  (lsp-bridge-enable-debug nil)
  (lsp-bridge-enable-log nil)
  (lsp-bridge-enable-org-babel t)
  (lsp-bridge-org-babel-lang-list '("bash" "elisp" "go")))

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

(use-package prettier
  :hook
  ((js-mode . prettier-mode)
   (typescript-ts-mode . prettier-mode)
   (tsx-ts-mode . prettier-mode)))

(use-package treesit
  :commands treesit-font-lock-rules treesit-font-lock-recompute-features
  :config
  (add-to-list 'treesit-extra-load-path (concat emacs-data-dir "treesit/"))
  :hook
  (c-ts-mode
   .
   (lambda()
     (setq-local treesit-font-lock-level 4)
     (setq-local
      treesit-font-lock-settings
      (append
       treesit-font-lock-settings
       (treesit-font-lock-rules
        :language 'c
        :feature 'func
        '((call_expression
           function:
           (identifier) @font-lock-property-face
           arguments: (_))))))))
  (java-ts-mode
   .
   (lambda()
     (setq-local
      treesit-font-lock-settings
      (append
       treesit-font-lock-settings
       (treesit-font-lock-rules
        :language 'java
        :feature 'expression
        :override t
        '((method_invocation
           name: (identifier) @font-lock-property-face)))))))
  ;; (go-ts-mode
  ;;  .
  ;;  (lambda()
  ;;    (setq-local treesit-font-lock-level 4)
  ;;    (treesit-font-lock-recompute-features '(attribute import func))
  ;;    (setq-local
  ;;     treesit-font-lock-settings
  ;;     (append
  ;;      treesit-font-lock-settings
  ;;      (treesit-font-lock-rules
  ;;       :language 'go
  ;;       :feature 'import
  ;;       :override t
  ;;       '((import_declaration (import_spec_list (import_spec path: (interpreted_string_literal) @font-lock-constant-face))))

  ;;       :language 'go
  ;;       :feature 'func
  ;;       :override t
  ;;       "[(function_declaration name: (identifier) @font-lock-function-name-face)
  ;;         (call_expression
  ;;           function:
  ;;           (selector_expression
  ;;            field: (field_identifier) @font-lock-property-face .))
  ;;         (call_expression
  ;;           function:
  ;;           (identifier) @font-lock-property-face
  ;;           arguments: (_))
  ;;         (method_declaration
  ;;           name: (field_identifier) @font-lock-function-name-face)]"

  ;;       :language 'go
  ;;       :feature 'variable
  ;;       :override t
  ;;       '((const_declaration
  ;;        (const_spec name: (identifier) @font-lock-constant-face)))

  ;;       :language 'go
  ;;       :feature 'attribute
  ;;       :override t
  ;;       "(composite_literal body: (literal_value (keyed_element . (literal_element (identifier) @font-lock-property-face))))")))))
  :custom
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
     (conf-toml-mode . toml-ts-mode)
     (yaml-mode . yaml-ts-mode))))

(use-package docker-compose-mode)

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

(defun ansi-color-apply-compilation-buffer ()
  "Apply ansi color for compilation buffer."
  (read-only-mode nil)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (read-only-mode))

(add-hook 'compilation-filter-hook 'ansi-color-apply-compilation-buffer)

;;; 30-prog.el ends here
;;; End:
