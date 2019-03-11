;;; site-start.el --- Site start
;;; Commentary:

;;; Code:
;; 去掉 Emacs 和 gnus 启动时的引导界面
(setq inhibit-startup-message t)

(virtualstudio-load-pyim)
(virtualstudio-initialize)

(server-start)

;;; site-start.el ends here
;;; End:
