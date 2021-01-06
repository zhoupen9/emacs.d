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
  :custom (dashboard-page-separator "\n\n")
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
       "Browse GitHub Homepage"
       (lambda (&rest _) (browse-url "https://github.com/zhoupen9")))
            (,(all-the-icons-octicon "bug" :height 1.1 :v-adjust 0.0)
       "Jira"
       "Browse JIRA"
       (lambda (&rest _) (browse-url "https://jira2.nroad.com.cn"))))))

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

(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

(use-package org
  :custom-face
  (org-link ((t (:underline t :foreground "#2aa1ae"))))
  (org-table ((t (:inherit fixed-pitch :background "#182232"))))
  (org-code ((t (:inherit fixed-pitch :foreground "#289ed0"))))
  (org-meta-line ((t (:inherit variable-pitch))))
  (org-document-info ((t (:inherit fixed-pitch))))
  (org-document-info-keyword ((t (:inherit variable-pitch))))
  (org-verbatim ((t (:inherit fixed-pitch :foreground "#bc6ec5"))))
  (org-block ((t (:inherit fixed-pitch))))
  (org-block-begin-line ((t (:inherit fixed-pitch))))
  (org-block-end-line ((t (:inherit fixed-pitch)))))

(defun ui-article-mode ()
  "Define article mode."
  (variable-pitch-mode t)
  (setq-local line-spacing 0.15))

(defun ui-prog-mode ()
  "Prog mode ui."
  (setq-local line-spacing 0.1))

(use-package markdown-mode
  :custom-face
  (markdown-table-face ((t (:inherit fixed-pitch))))
  :custom
  (markdown-hide-markup t)
  (markdown-fontify-code-blocks-natively t))

(use-package face-remap
  :hook
  (org-mode . ui-article-mode)
  (markdown-mode . ui-article-mode)
  (prog-mode . ui-prog-mode)
  :custom-face
  (fixed-pitch ((t (:family "Liberation Mono" :height 0.9))))
  (variable-pitch ((t (:family "Arial" :height 1.15)))))

;; (load-theme 'spacemacs-dark t)
(use-package spacemacs-common
  :ensure spacemacs-theme
  :config
  (load-theme 'spacemacs-dark t)
  :custom-face
  ;; line number
  (line-number-current-line ((t (:inherit line-number :background "#106040" :foreground "#b2b2b2"))))
  (link ((t (:foreground "#2aa1ae"))))
  ;; hightlight line
  (hl-line ((t (:background "#242628"))))
  ;; mode line
  (mode-line ((t (:background "#242628" :foreground "#b2b2b2" :box (:line-width 1 :color "#26282a")))))
  (mode-line-inactive ((t (:background "#1a1a1a" :foreground "#8a8a8a" :box (:line-width 1 :color "#1f1f1f")))))
  :custom
  ;; disable spacemacs comment background
  (spacemacs-theme-comment-bg nil)
  ;; custom theme colors
  (spacemacs-theme-custom-colors
   (if (display-graphic-p)
       '((bg1 . "#171a1f")
         (bg2 . "#14141a")
         (comment . "#6a6a70")
         (comment-bg . "#171a1f")
         (highlight . "#303038")
         (cblk-ln-bg . "#21212a")
         (cblk-bg . "#202028")
         (border . "#282828"))
     '((border . "#696969")))))

;;; 80-ui.el ends here
;;; End:
