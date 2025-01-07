;;; -*- lexical-binding: t; -*-
;;; 80-ui.el --- Personal Private UI
;;; Commentary:

;;; Code:

(customize-set-variable 'calendar-chinese-all-holidays-flag t)

;;;###autoload
(defun ui-comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line.
if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

;;;###autoload
(defun ui-beautify ()
  "Beautify buffer by run tabify and follow by ''indent-region' ."
  (interactive)
  ;; remove all trialing whitespaces.
  (whitespace-cleanup-region (point-min) (point-max))
  ;; translate spaces to tabs.
  (untabify (point-min) (point-max))
  ;; indent whole buffer.
  (indent-region (point-min) (point-max))
  ;; print message done.
  (message "Beautify buffer done."))

;; spacemacs
(use-package spacemacs-theme
  :ensure spacemacs-theme
  :config
  (load-theme 'spacemacs-dark t)
  :custom-face
  ;; line number
  (line-number-current-line ((t (:inherit line-number :background "#22283a" :foreground "#b2b2b2"))))
  (link ((t (:foreground "#2aa1ae"))))
  ;; hightlight line
  ;;(hl-line ((t (:background "#242628"))))
  (hl-line ((t (:background "#1a1c1d"))))
  ;; mode line
  (mode-line ((t (:background "#282a30" :foreground "#b2b2b2" :box (:line-width (1 . 1) :color "#181820")))))
  (mode-line-inactive ((t (:background "#222228" :foreground "#8a8a8a" :box (:line-width (1 . 1) :color "#181820")))))
  ;;(isearch ((t (:background "green3"))))
  (lazy-highlight ((t (:background "grey19" :box (:line-width (-1 . -1) :color "grey35")))))
  :custom
  ;; disable spacemacs comment background
  (spacemacs-theme-comment-bg nil)
  ;; custom theme colors
  (spacemacs-theme-custom-colors
   (if (display-graphic-p)
       '((base . "#d2d2d2")
         (bg1 . "#171a1f")
         (bg2 . "#14141a")
         (comment . "#6a6a70")
         (comment-bg . "#171a1f")
         (highlight . "#303038")
         (cblk-ln-bg . "#21212a")
         (cblk-bg . "#202028")
         (border . "#282828"))
     '((border . "#696969")
       (base . "#d2d2d2")
       (bg1 . "#171a1f")
       (bg2 . "#14141a")
       (comment . "#6a6a70")
       ))))

(use-package doom-modeline
  :config
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (setq doom-modeline-bar-width 2)
  (setq doom-modeline-height 25)
  :custom-face
  (doom-modeline-bar ((t (:background "#fc6e55"))))
  (doom-modeline-bar-inactive ((t (:background "#666666"))))
  :init
  (doom-modeline-mode t))

(use-package frame
  :demand
  :config
  (add-hook 'after-init-hook (lambda() (set-cursor-color "#fa6422"))))

(use-package all-the-icons)

(use-package nerd-icons)

(use-package all-the-icons-nerd-fonts
  :after all-the-icons
  :config
  (all-the-icons-nerd-fonts-prefer))

(use-package nerd-icons-completion
  :config
  (nerd-icons-completion-mode))

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :bind
  (("C-x x d" . dirvish))
  :config
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes
        '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg)))

(use-package dashboard
  :demand
  :commands nerd-icons-faicon nerd-icons-fileicon nerd-icons-octicon
  :after nerd-icons
  :config
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-page-separator "\n\n")
  (dashboard-display-icons-p t)
  (dashboard-icon-type 'nerd-icons)

  (dashboard-navigator-buttons
   `(
     ((,(nerd-icons-faicon "nf-fa-gitlab" :height 1.1 :v-adjust 0.0)
       "GitLab"
       "Browse GitLab Repositories"
       (lambda (&rest _) (browse-url "https://gitlab.nroad.com.cn")))
      (,(nerd-icons-codicon "nf-cod-repo" :height 1.1 :v-adjust 0.0)
       "Nexus"
       "Browse Nexus Repositories"
       (lambda (&rest _) (browse-url "https://nexus.nroad.com.cn")))
      (,(nerd-icons-devicon "nf-dev-jenkins" :height 1.1 :v-adjust 0.0)
       "Jenkins CI"
       "Browse Jenkins CI Jobs"
       (lambda (&rest _) (browse-url "https://jenkins.nroad.com.cn"))))
     ((,(nerd-icons-faicon "nf-fa-github" :height 1.1 :v-adjust 0.0)
       "Homepage"
       "Browse GitHub Homepage"
       (lambda (&rest _) (browse-url "https://github.com/zhoupen9")))
      (,(nerd-icons-faicon "nf-fa-bug" :height 1.1 :v-adjust 0.0)
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
  ;; (org-meta-line ((t (:inherit variable-pitch :height 0.9))))
  (org-meta-line ((t (:inherit variable-pitch))))
  (org-document-info ((t (:inherit fixed-pitch))))
  (org-document-info-keyword ((t (:inherit variable-pitch))))
  ;; (org-verbatim ((t (:inherit fixed-pitch :foreground "#bc6ec5" :height 0.94))))
  ;; (org-block ((t (:inherit fixed-pitch :height 0.94))))
  ;; (org-block-begin-line ((t (:inherit fixed-pitch :height 0.94))))
  ;; (org-block-end-line ((t (:inherit fixed-pitch :height 0.94)))))
  (org-verbatim ((t (:inherit fixed-pitch :foreground "#bc6ec5"))))
  (org-block ((t (:inherit fixed-pitch))))
  (org-block-begin-line ((t (:inherit fixed-pitch))))
  (org-block-end-line ((t (:inherit fixed-pitch)))))

(use-package markdown-mode
  :custom-face
  (markdown-table-face ((t (:inherit fixed-pitch :background "#182232")))))

(defun ui-article-mode ()
  "Define article mode."
  (variable-pitch-mode t)
  ;;(setq-local line-spacing 0.15)
  (setq-local line-spacing 0.2)
  (face-remap-add-relative 'fixed-pitch :height 0.9)
  (face-remap-add-relative 'variable-pitch :height 1.1))

(defun ui-prog-mode ()
  "Prog mode ui."
  ;;(setq-local line-spacing 0.15)
  (setq-local line-spacing 0.2)
  (face-remap-add-relative 'font-lock-function-call-face :slant 'italic :weight 'normal)
  (face-remap-add-relative 'font-lock-variable-use-face :foreground "#f2f2f2")
  (face-remap-add-relative 'font-lock-variable-name-face :foreground "#f2f2f2")
  (face-remap-add-relative 'font-lock-string-face :foreground "#77aa99")
  (face-remap-add-relative 'font-lock-type-face :weight 'normal :foreground "#4f97d7")
  (face-remap-add-relative 'font-lock-keyword-face :weight 'normal)
  (face-remap-add-relative 'font-lock-property-face :foreground "#7590db")
  (face-remap-add-relative 'font-lock-function-name-face :slant 'italic :weight 'normal))

(use-package face-remap
  :hook
  (org-mode . ui-article-mode)
  (markdown-mode . ui-article-mode)
  (prog-mode . ui-prog-mode)
  (go-ts-mode . ui-prog-mode)
  (java-ts-mode . ui-prog-mode)
  :custom-face
  ;;(fixed-pitch ((t :family "LiterationMono Nerd Font Mono")))
  (fixed-pitch ((t :family "Monaspace Neon")))
  ;;(fixed-pitch ((t :family "Fantasque Sans Mono")))
  ;;(variable-pitch ((t :family "Source Serif Pro"))))
  (variable-pitch ((t :family "Inter"))))

;;; 80-ui.el ends here
;;; End:
