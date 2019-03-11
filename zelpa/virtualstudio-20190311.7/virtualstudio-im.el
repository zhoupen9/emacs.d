;;; virtualstudio-im.el --- VirutalStudio input method configurations
;;; Commentary:
;;; VirtualStudio input method

;;; Code:
(defun virtualstudio-load-pyim-if-gnu-linux ()
  "Load pyim input methods if running in gnu/linux."
  (require 'use-package)
  (when (eq system-type 'gnu/linux)
    (use-package pyim
      :ensure nil
      :demand t
      :config
      ;; 激活 basedict 拼音词库
      (use-package pyim-basedict
        :ensure nil
        :commands pyim-basedict-enable
        :config
        (pyim-basedict-enable))
      (setq default-input-method "pyim")
      ;; 使用全拼
      (setq pyim-default-scheme 'quanpin)
      ;; ;; 开启拼音搜索功能
      ;; (pyim-isearch-mode 1)
      (setq pyim-page-tooltip 'posframe)
      ;; (setq pyim-page-tooltip 'popup)
      ;; 选词框显示5个候选词
      (setq pyim-page-length 5))))

(provide 'virtualstudio-im)
;;; virtualstudio-im.el ends here

