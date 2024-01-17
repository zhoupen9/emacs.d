;;; early-init.el --- Emacs 27 early init
;;; Commentary:

;;; Code:

(let* ((xdg-data-dir (expand-file-name (file-name-as-directory "~/.local/share")))
       (local-site-lisp (concat xdg-data-dir "emacs/site-lisp")))
  (customize-set-variable 'package-user-dir (concat xdg-data-dir "emacs/elpa/"))
  (customize-set-variable 'package-gnupghome-dir (concat xdg-data-dir "emacs/elpa/gnupg/"))
  (when (file-directory-p local-site-lisp)
    (add-to-list 'load-path local-site-lisp)))

(customize-set-variable
 'package-archives
 ;; '(("gnu-cn"   . "https://mirrors.ustc.edu.cn/elpa/gnu/")
 ;;   ("melpa-cn" . "https://mirrors.ustc.edu.cn/elpa/melpa/")))
 ;; '(("gnu-cn"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
 ;;   ("melpa-cn" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
 '(("gnu"   . "https://elpa.gnu.org/packages/")
   ("melpa" . "https://melpa.org/packages/")))

;; (setq url-proxy-services
;;       '(("no_proxy" . "^\\(localhost\\|10.*\\|172.16.*\\|192.168.*\\|*.nroad.com.cn\\)")
;;         ("http" . "localhost:18123")
;;         ("https" . "localhost:18123")))

;; (setq url-http-proxy-basic-auth-storage
;;       (list (list "proxy.com:8080"
;;                   (cons "Input your LDAP UID !"
;;                         (base64-encode-string "LOGIN:PASSWORD")))))

(defconst emacs-data-dir (expand-file-name (file-name-as-directory "~/.var/lib/emacs")) "Emacs data directory.")

;; Defines home-directory
(defconst home-directory (expand-file-name (file-name-as-directory "~")) "User home direcotry.")

;; Defines cache directory
(defconst emacs-cache-dir (expand-file-name (file-name-as-directory "~/.cache/emacs")) "Emacs cache directory.")

(setq native-comp-eln-load-path (list (concat emacs-cache-dir "eln-cache")))

;;; early-init.el ends here
