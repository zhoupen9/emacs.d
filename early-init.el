;;; early-init.el --- Emacs 27 early init
;;; Commentary:

;;; Code:

(let* ((xdg-data-dir "~/.local/share")
       (local-site-lisp (concat xdg-data-dir "/emacs/site-lisp")))
  (customize-set-variable 'package-user-dir (concat xdg-data-dir "/emacs/elpa/"))
  (customize-set-variable 'package-gnupghome-dir (concat xdg-data-dir "/emacs/elpa/gnupg/"))
  (when (file-directory-p local-site-lisp)
    (add-to-list 'load-path local-site-lisp)))

(customize-set-variable
 'package-archives
 '(("gnu-cn"   . "https://mirrors.ustc.edu.cn/elpa/gnu/")
   ("melpa-cn" . "https://mirrors.ustc.edu.cn/elpa/melpa/")))
;; '(("gnu-cn"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;;   ("melpa-cn" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(defconst emacs-data-dir (expand-file-name (file-name-as-directory "~/.var/lib/emacs")) "Emacs data directory.")

;; Defines home-directory
(defconst home-directory (expand-file-name (file-name-as-directory "~")) "User home direcotry")

;;; early-init.el ends here
