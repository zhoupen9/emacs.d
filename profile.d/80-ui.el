;;; 80-ui.el --- Personal Private UI
;;; Commentary:

;;; Code:

(customize-set-variable 'calendar-chinese-all-holidays-flag t)

;;;###autoload
(defun ui-comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

;;;###autoload
(defun ui-beautify ()
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

(defun ui--set-org-buffer-variable-pitch ()
  "Set buffer variable pitch."
  (interactive)
  (variable-pitch-mode t)
  ;; (setq line-spacing 3)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch))
;;  (set-face-attribute 'org-block-background nil :inherit 'fixed-pitch))

(defun ui--set-markdown-buffer-variable-pitch ()
  "Set markdown buffer variable pitch."
  (interactive)
  (variable-pitch-mode t)
  (set-face-attribute 'markdown-table-face nil :inherit 'fixed-pitch))

(defun ui--set-window-titlebar-theme-variant (variant)
  "Change emacs-gtk title bars theme VARIANT."
  (interactive "sTheme Variant Name: ")
  (let ((pid (replace-regexp-in-string  "\n$" "" (shell-command-to-string "pidof emacs"))))
    (let ((cmd (concat "wmctrl -lp | grep " pid " | awk '{ print $1 }'")))
      (let ((ids (replace-regexp-in-string "\n$" " " (shell-command-to-string cmd))))
        (dolist (id (split-string ids))
          (shell-command
           (concat "xprop -id " id " -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT " variant)))))))

(defun ui--dark-idea ()
  "Change idea title bars theme to dark."
  (interactive)
  (let ((pid (replace-regexp-in-string  "\n$" "" (shell-command-to-string "jps | grep Main | awk '{ print $1 }'"))))
    (let ((cmd (concat "wmctrl -lp | grep " pid " | awk '{ print $1 }'")))
      (let ((ids (replace-regexp-in-string "\n$" " " (shell-command-to-string cmd))))
        (dolist (id (split-string ids))
          (shell-command
           (concat "xprop -id " id " -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT dark")))))))

(defun ui--dark-flatpak-intellij-idea ()
  "Change idea title bars theme to dark."
  (interactive)
  (let ((cmd (concat "wmctrl -lp | grep -e \"IntelliJ IDEA$\" | awk '{ print $1 }'")))
    (let ((ids (replace-regexp-in-string "\n$" " " (shell-command-to-string cmd))))
      (dolist (id (split-string ids))
        (shell-command
         (concat "xprop -id " id " -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT dark"))))))

(defun ui--dark-titlebar ()
  "Change title bar to dark theme."
  (interactive)
  (when (eq system-type 'darwin)
    (add-to-list 'default-frame-alist '(ns-appearance . dark)))
  (when (eq system-type 'gnu/linux)
    (ui--set-window-titlebar-theme-variant "dark")))

(use-package doom-modeline
  :config
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)
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

(use-package dashboard
  :commands all-the-icons-faicon all-the-icons-fileicon all-the-icons-octicon
  :after all-the-icons
  :custom
  (dashboard-navigator-buttons
   `(
     ((,(all-the-icons-faicon "gitlab" :height 1.1 :v-adjust 0.0)
       "GitLab"
       "Browse GitLab Repositories"
       (lambda (&rest _) (browse-url "https://gitlab.nroad.com.cn")))
      (,(all-the-icons-fileicon "npm" :height 1.1 :v-adjust 0.0)
       "Nexus"
       "Browse Nexus Repositories"
       (lambda (&rest _) (browse-url "https://nexus.nroad.com.cn")))
      (,(all-the-icons-fileicon "jenkins" :height 1.1 :v-adjust 0.0)
       "Jenkins CI"
       "Browse Jenkins CI Jobs"
       (lambda (&rest _) (browse-url "https://jenkins.nroad.com.cn"))))
     ((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
       "Homepage"
       "Browse homepage"
       (lambda (&rest _) (browse-url "https://github.com/zhoupen9"))))))

  (dashboard-center-content t)
  (dashboard-items
   '((recents  . 5)
     (bookmarks . 5)
     (projects . 5)
     (agenda . 5)
     (registers . 5)))
  (dashboard-startup-banner (concat user-emacs-directory "themes/icons/spacemacs-app.png"))
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-init-info t)
  (dashboard-set-navigator t))

(load-theme 'spacemacs-dark t)
;;; 80-ui.el ends here
;;; End:
