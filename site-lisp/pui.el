;;; pui.el --- Personal Private UI
;;; Commentary:

;;; Code:

;;;###autoload
(defun pui-comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

;;;###autoload
(defun pui-beautify ()
  "Beautify buffer by run tabify and follow by 'indent-region."
  (interactive)
  ;; remove all trialing whitespaces.
  (whitespace-cleanup-region (point-min) (point-max))
  ;; translate spaces to tabs.
  (untabify (point-min) (point-max))
  ;; indent whole buffer.
  (indent-region (point-min) (point-max))
  ;; print message done.
  (message "Beautify buffer done."))

(defun pui--set-org-buffer-variable-pitch ()
  "Set buffer variable pitch."
  (interactive)
  (variable-pitch-mode t)
  ;; (setq line-spacing 3)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch))
;;  (set-face-attribute 'org-block-background nil :inherit 'fixed-pitch))

(defun pui--set-markdown-buffer-variable-pitch ()
  "Set markdown buffer variable pitch."
  (interactive)
  (variable-pitch-mode t)
  (set-face-attribute 'markdown-table-face nil :inherit 'fixed-pitch))

(defun pui--set-window-titlebar-theme-variant (variant)
  "Change emacs-gtk title bars theme VARIANT."
  (interactive "sTheme Variant Name: ")
  (let ((pid (replace-regexp-in-string  "\n$" "" (shell-command-to-string "pidof emacs"))))
    (let ((cmd (concat "wmctrl -lp | grep " pid " | awk '{ print $1 }'")))
      (let ((ids (replace-regexp-in-string "\n$" " " (shell-command-to-string cmd))))
        (dolist (id (split-string ids))
          (shell-command
           (concat "xprop -id " id " -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT " variant)))))))

(defun pui--dark-idea ()
  "Change idea title bars theme to dark."
  (interactive)
  (let ((pid (replace-regexp-in-string  "\n$" "" (shell-command-to-string "jps | grep Main | awk '{ print $1 }'"))))
    (let ((cmd (concat "wmctrl -lp | grep " pid " | awk '{ print $1 }'")))
      (let ((ids (replace-regexp-in-string "\n$" " " (shell-command-to-string cmd))))
        (dolist (id (split-string ids))
          (shell-command
           (concat "xprop -id " id " -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT dark")))))))

(defun pui--dark-flatpak-intellij-idea ()
  "Change idea title bars theme to dark."
  (interactive)
  (let ((cmd (concat "wmctrl -lp | grep -e \"IntelliJ IDEA$\" | awk '{ print $1 }'")))
    (let ((ids (replace-regexp-in-string "\n$" " " (shell-command-to-string cmd))))
      (dolist (id (split-string ids))
        (shell-command
         (concat "xprop -id " id " -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT dark"))))))

(defun pui--dark-titlebar ()
  "Change title bar to dark theme."
  (interactive)
  (when (eq system-type 'darwin)
    (add-to-list 'default-frame-alist '(ns-appearance . dark)))
  (when (eq system-type 'gnu/linux)
    (pui--set-window-titlebar-theme-variant "dark")))

;; ;; Load pyim input methods if running in gnu/linux.
;; (use-package pyim
;;   :ensure nil
;;   :demand t
;;   :config
;;   ;; use basedict
;;   (use-package pyim-basedict
;;     :ensure nil
;;     :commands pyim-basedict-enable
;;     :config
;;     (pyim-basedict-enable))
;;   (setq default-input-method "pyim")
;;   ;; Enable "Quanpin"
;;   (setq pyim-default-scheme 'quanpin)
;;   ;; ;; Enable pinyin search
;;   ;; (pyim-isearch-mode 1)
;;   (setq pyim-page-tooltip 'posframe)
;;   ;; (setq pyim-page-tooltip 'popup)
;;   ;; set candicates size
;;   (setq pyim-page-length 5)
;;   (setq pyim-dicts
;;    (quote
;;     ((:name "bigdict" :file "~/.emacs.d/pyim/dicts/pyim-bigdict.pyim")))))

(use-package doom-modeline
  :config
  (doom-modeline-mode t))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook))

(use-package beacon
  :custom
  (beacon-push-mark 10)
  (beacon-color "#cc342b")
  (beacon-blink-delay 0.3)
  (beacon-blink-duration 0.3)
  :config
  (beacon-mode))

(use-package frame
  :demand
  :config
  (add-hook 'after-init-hook (lambda() (set-cursor-color "#fa6422"))))

(use-package hl-line
  :config
  (add-hook 'after-init-hook (lambda() (set-face-background hl-line-face "#502040")))
  (global-hl-line-mode 1))

(provide 'pui)
;;; pui.el ends here
;;; End:
