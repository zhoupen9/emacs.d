;;; zoe.el --- Zhou Peng's own simple macros
;;; Commentary:
;;; Simple macros

;;; Code:

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defun beautify ()
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

;; Define a skip buffer list.
(defconst skip-buffers '("*Messages*" "*scratch*" "*Help*" "*Completions*"))

(defun switch-to-next-active-buffer ()
  "Switch to next buffer but skipping scratch buffers."
  (interactive)
  (next-buffer)
  (while (member (buffer-name) skip-buffers)
    (next-buffer)))

(defun switch-to-previous-active-buffer ()
  "Switch to previous buffer but skipping scratch buffers."
  (interactive)
  (previous-buffer)
  (while (member (buffer-name) skip-buffers)
    (previous-buffer)))

;; binding key control-\ to comment/uncomment.
(global-set-key (kbd "C-;") 'comment-or-uncomment-region-or-line)

;; binding key control-c control-e to beautify.
(global-set-key (kbd "C-c b f") 'beautify)

;; remap next-buffer and previous buffer
;; (global-set-key [remap next-buffer] 'switch-to-next-active-buffer)
;; (global-set-key [remap previous-buffer] 'switch-to-previous-active-buffer)

(defun set-window-titlebar-theme-variant (variant)
  "Change emacs-gtk title bars theme VARIANT."
  (interactive "sTheme Variant Name: ")
  (let ((pid (replace-regexp-in-string  "\n$" "" (shell-command-to-string "pidof emacs"))))
    (let ((cmd (concat "wmctrl -lp | grep " pid " | awk '{ print $1 }'")))
      (let ((ids (replace-regexp-in-string "\n$" " " (shell-command-to-string cmd))))
        (dolist (id (split-string ids))
          (shell-command
           (concat "xprop -id " id " -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT " variant)))))))

(defun dark-idea ()
  "Change idea title bars theme to dark."
  (interactive)
  (let ((pid (replace-regexp-in-string  "\n$" "" (shell-command-to-string "jps | grep Main | awk '{ print $1 }'"))))
    (let ((cmd (concat "wmctrl -lp | grep " pid " | awk '{ print $1 }'")))
      (let ((ids (replace-regexp-in-string "\n$" " " (shell-command-to-string cmd))))
        (dolist (id (split-string ids))
          (shell-command
           (concat "xprop -id " id " -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT dark")))))))

(defun dark-flatpak-intellij-idea ()
  "Change idea title bars theme to dark."
  (interactive)
  (let ((cmd (concat "wmctrl -lp | grep -e \"IntelliJ IDEA$\" | awk '{ print $1 }'")))
    (let ((ids (replace-regexp-in-string "\n$" " " (shell-command-to-string cmd))))
      (dolist (id (split-string ids))
        (shell-command
         (concat "xprop -id " id " -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT dark"))))))

(require 'org)
(defun set-org-buffer-variable-pitch ()
    "Set buffer variable pitch."
    (interactive)
    (variable-pitch-mode t)
    ;(setq line-spacing 3)
    (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-block-background nil :inherit 'fixed-pitch))

(defun set-markdown-buffer-variable-pitch ()
  "Set markdown buffer variable pitch."
  (interactive)
  (variable-pitch-mode t)
  (set-face-attribute 'markdown-table-face nil :inherit 'fixed-pitch))

(defun dark-titlebar ()
  "Change title bar to dark theme."
  (when (eq system-type 'darwin)
    (add-to-list 'default-frame-alist '(ns-appearance . dark)))

  (when (eq system-type 'gnu/linux)
    (set-window-titlebar-theme-variant "dark")))

(provide 'zoe)
;;; zoe ends here

