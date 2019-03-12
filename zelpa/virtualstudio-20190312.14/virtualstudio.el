;;; virtualstudio.el --- Virutal Studio configurations
;;; Commentary:

;;; Code:

(require 'company)
(require 'company-quickhelp)
(require 'company-rtags)
(require 'flycheck-pyflakes)
(require 'flycheck-yamllint)
(require 'go-mode)
(require 'go-eldoc)
(require 'jedi-core)
(require 'python-mode)

(defun virtualstudio-setup-helm ()
  "Setup Virtual Studio helm."
  (helm-mode t)
  (helm-projectile-on))

(defun virtualstudio-setup-prog-mode-hooks ()
  "Setup virtual studio programming mode hooks."
  ;; display line number in programing modes
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook 'prog-mode-hook 'company-mode)
  (add-hook 'prog-mode-hook 'company-quickhelp-mode))

(defun virtualstudio-setup-prog-env ()
  "Setup virtual studio programming environtment."
  (virtualstudio-setup-prog-mode-hooks)
  
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

  (projectile-mode t)

  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)


  (add-to-list 'company-backends 'company-rtags)
  (global-set-key (kbd "C-c c c") 'company-complete)

  ;;Initialize exec-path-from-shell
  (exec-path-from-shell-initialize)

  (global-flycheck-mode t)

  (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

  (add-hook 'go-mode-hook 'flycheck-mode)
  (add-hook 'flycheck-mode-hook 'flycheck-golangci-lint-setup)

  (add-hook 'go-mode-hook 'go-eldoc-setup)
  (add-hook 'go-mode-hook
            (lambda()
              (add-to-list 'company-backends 'company-go)))

  (add-hook 'python-mode-hook
            (lambda ()
              (add-to-list 'company-backends 'company-jedi)))
  ;; (setq jedi:complete-on-dot t)
  ;; (setq jedi:tooltip-method nil)
  ;; (setq jedi:get-in-function-call-delay 0)))
  (add-hook 'python-mode-hook 'flycheck-mode)
  (put 'flycheck-clang-args 'safe-local-variable-values (lambda(xx) t))

  (add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup))

(defun virtualstudio-setup-key-bindings ()
  "Setup virtual studio key bindings."
  ;; key bindings
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-c r f") 'rtags-find-symbol)
  (global-set-key (kbd "C-c r c") 'rtags-find-symbol-at-point)
  (global-set-key (kbd "C-c r p") 'rtags-print-symbol-info)
  (global-set-key (kbd "C-c r b") 'rtags-location-stack-back)
  ;; binding key control-\ to comment/uncomment.
  (global-set-key (kbd "C-;") 'virtualstudio-comment-or-uncomment-region-or-line)
  ;; binding key control-c control-e to beautify.
  (global-set-key (kbd "C-c b f") 'virtualstudio-beautify))
;; remap next-buffer and previous buffer
;; (global-set-key [remap next-buffer] 'switch-to-next-active-buffer)
;; (global-set-key [remap previous-buffer] 'switch-to-previous-active-buffer)

;;;###autoload
(defun virtualstudio-initialize ()
  "Initialize virtual studio."
  (interactive)

  (virtualstudio-setup-key-bindings)

  (virtualstudio-setup-helm)

  (virtualstudio-setup-prog-env)
  
  ;;(require 'virtualstudio-gui)
  (virtualstudio-setup-gui)

  ;; load spacemacs dark theme
  (load-theme 'spacemacs-dark t))

(provide 'virtualstudio)
;;; virtualstudio.el ends here
