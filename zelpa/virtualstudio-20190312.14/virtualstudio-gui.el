;;; virtualstudio-gui.el --- VirutalStudio GUI configurations
;;; Commentary:
;;; VirtualStudio

;;; Code:
(require 'org)
(require 'markdown-mode)

;;;###autoload
(defun virtualstudio-setup-gui-defaults ()
  "Set graphic interface defaults."
  ;; change title bar display
  (setq frame-title-format '("" "[%b] %f - Emacs " emacs-version))

  ;; Set font color
  (global-font-lock-mode t)

  ;; Hilight selection
  ;; (pc-selection-mode)

  ;; time format
  (display-time)
  ;; (setq display-time-24hr-format t)
  ;;(setq display-time-day-and-date t)

  ;; set 'yes or no' to 'y or n'
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; highlight marked block
  (transient-mark-mode t)

  ;; display line and column number
  (column-number-mode t)
  (line-number-mode t)

  ;; show paren
  (show-paren-mode t)

  ;; shutdown bell
  (setq visible-bell t)

  ;; set tab stop to 4.
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  ;;(setq tab-stop-list (number-sequence 4 120 4))

  ;; set backup dir
  ;;(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

  ;; Enable electric minor mode
  (electric-indent-mode t)

  ;; Add confirmation for quit emacs
  (setq confirm-kill-emacs 'yes-or-no-p))

(defun virtualstudio-set-window-titlebar-theme-variant (variant)
  "Change emacs-gtk title bars theme VARIANT."
  (interactive "sTheme Variant Name: ")
  (let ((pid (replace-regexp-in-string  "\n$" "" (shell-command-to-string "pidof emacs"))))
    (let ((cmd (concat "wmctrl -lp | grep " pid " | awk '{ print $1 }'")))
      (let ((ids (replace-regexp-in-string "\n$" " " (shell-command-to-string cmd))))
        (dolist (id (split-string ids))
          (shell-command
           (concat "xprop -id " id " -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT " variant)))))))

;;;###autoload
(defun virtualstudio-dark-idea ()
  "Change idea title bars theme to dark."
  (interactive)
  (let ((pid (replace-regexp-in-string  "\n$" "" (shell-command-to-string "jps | grep Main | awk '{ print $1 }'"))))
    (let ((cmd (concat "wmctrl -lp | grep " pid " | awk '{ print $1 }'")))
      (let ((ids (replace-regexp-in-string "\n$" " " (shell-command-to-string cmd))))
        (dolist (id (split-string ids))
          (shell-command
           (concat "xprop -id " id " -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT dark")))))))

;;;###autoload
(defun virtualstudio-dark-flatpak-intellij-idea ()
  "Change idea title bars theme to dark."
  (interactive)
  (let ((cmd (concat "wmctrl -lp | grep -e \"IntelliJ IDEA$\" | awk '{ print $1 }'")))
    (let ((ids (replace-regexp-in-string "\n$" " " (shell-command-to-string cmd))))
      (dolist (id (split-string ids))
        (shell-command
         (concat "xprop -id " id " -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT dark"))))))

(defun virtualstudio-set-org-buffer-variable-pitch ()
  "Set buffer variable pitch."
  (interactive)
  (variable-pitch-mode t)
  ;; (setq line-spacing 3)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-block-background nil :inherit 'fixed-pitch))

(defun virtualstudio-set-markdown-buffer-variable-pitch ()
  "Set markdown buffer variable pitch."
  (interactive)
  (variable-pitch-mode t)
  (set-face-attribute 'markdown-table-face nil :inherit 'fixed-pitch))

;;;###autoload
(defun virtualstudio-dark-titlebar ()
  "Change title bar to dark theme."
  (interactive)
  (when (eq system-type 'darwin)
    (add-to-list 'default-frame-alist '(ns-appearance . dark)))

  (when (eq system-type 'gnu/linux)
    (virtualstudio-set-window-titlebar-theme-variant "dark")))

;;;###autoload
(defun virtualstudio-setup-gui ()
  "Setup graphic interfaces."
  (virtualstudio-setup-gui-defaults)
  (virtualstudio-dark-titlebar)

  (add-hook 'org-mode-hook 'set-org-buffer-variable-pitch)
  (add-hook 'markdown-mode-hook 'set-markdown-buffer-variable-pitch)

  (when (display-graphic-p)
    ;; Turn off toolbar.
    (tool-bar-mode 0)
    ;; Turn off menubar.
    (menu-bar-mode 0)
    ;; Turn off scroll bar.
    (scroll-bar-mode 0)
    ;; Turn on speed bar.
    (speedbar-frame-mode t)
  
    (when (eq system-type 'gnu/linux)
      ;; Chinese Font
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font)
	  		              charset
			              (font-spec :family "Noto Sans CJK SC"))))))

(provide 'virtualstudio-gui)
;;; virtualstudio-gui.el ends here
