;;; 90-keybindings.el --- Key bindings
;;; Commentary:

;;; Code:
(global-set-key (quote [f12]) (quote mu4e))
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-c c l") 'ui-comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-c b f") 'ui-beautify)
(global-set-key (kbd "C-c C-h") 'delete-trailing-whitespace)
(global-set-key (kbd "C-c TAB") 'tabify)
(global-set-key (kbd "C-c u TAB") 'untabify)

;;; 90-keybindings.el ends here
;;; End:
