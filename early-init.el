;;; early-init.el --- Emacs 27 early init
;;; Commentary:

;;; Code:

(let (xdg-data-dir (getenv "XDG_DATA_HOME"))
  (if (not xdg-data-dir)
      (setq xdg-data-dir "~/.local/share/"))
  (customize-set-variable 'package-user-dir (concat xdg-data-dir "emacs/elpa/"))
  (customize-set-variable 'package-gnupghome-dir (concat xdg-data-dir "emacs/elpa/gnupg/")))

(customize-set-variable
 'package-archives
 '(("gnu-cn"   . "http://mirrors.ustc.edu.cn/elpa/gnu/")
   ("melpa-cn" . "http://mirrors.ustc.edu.cn/elpa/melpa/")))
;; (setq package-archives '(("gnu-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;; ("melpa-cn" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(defconst emacs-data-dir (expand-file-name (file-name-as-directory "~/.var/lib/emacs")) "Emacs data directory.")

;;; early-init.el ends here
