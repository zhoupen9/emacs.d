;;; early-init.el --- Emacs 27 early init
;;; Commentary:

;;; Code:

;; set up package repositories.
(require 'package)
(let (xdg-data-dir (getenv "XDG_DATA_HOME"))
  (if (not xdg-data-dir)
      (setq xdg-data-dir "~/.local/share/"))
  (setq package-user-dir (concat xdg-data-dir  "emacs/elpa")
        package-gnupghome-dir (concat xdg-data-dir "emacs/elpa/gnupg")))

(defconst emacs-data-dir (expand-file-name (file-name-as-directory "~/.var/lib/emacs")) "Emacs data directory.")

;;; early-init.el ends here
