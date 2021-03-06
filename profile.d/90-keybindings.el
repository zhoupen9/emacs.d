;;; 90-keybindings.el --- Key bindings
;;; Commentary:

;;; Code:
(global-set-key (quote [f12]) (quote mu4e))
(global-set-key (kbd "M-3") 'other-window)
(global-set-key (kbd "C-c c l") 'ui-comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-c b f") 'ui-beautify)
(global-set-key (kbd "C-c s") 'eshell)
(global-set-key (kbd "<C-M-backspace>") 'delete-trailing-whitespace)

;;; 90-keybindings.el ends here
;;; End:
