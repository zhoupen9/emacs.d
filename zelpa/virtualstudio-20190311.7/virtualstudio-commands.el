;;; virtualstudio-gui.el --- VirutalStudio GUI configurations
;;; Commentary:
;;; VirtualStudio

;;; Code:
(defun virtualstudio-comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defun virtualstudio-beautify ()
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
(defconst virtualstudio-skip-buffers '("*Messages*" "*scratch*" "*Help*" "*Completions*"))

(defun virtualstudio-switch-to-next-active-buffer ()
  "Switch to next buffer but skipping scratch buffers."
  (interactive)
  (next-buffer)
  (while (member (buffer-name) virtualstudio-skip-buffers)
    (next-buffer)))

(defun virtualstudio-switch-to-previous-active-buffer ()
  "Switch to previous buffer but skipping scratch buffers."
  (interactive)
  (previous-buffer)
  (while (member (buffer-name) virtualstudio-skip-buffers)
    (previous-buffer)))

(provide 'virtualstudio-commands)
;;; virtualstudio-commands.el ends here
