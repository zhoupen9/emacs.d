;;; 60-im.el --- summary input method configurations
;;; Commentary:

;;; Code:

;; ;; Load pyim input methods if running in gnu/linux.
;; (use-package pyim
;;   :ensure nil
;;   :demand t
;;   :config
;;   ;; use basedict
;;   (use-package pyim-basedict
;;     :ensure nil
;;     :commands pyim-basedict-enable
;;     :config
;;     (pyim-basedict-enable))
;;   (setq default-input-method "pyim")
;;   ;; Enable "Quanpin"
;;   (setq pyim-default-scheme 'quanpin)
;;   ;; ;; Enable pinyin search
;;   ;; (pyim-isearch-mode 1)
;;   (setq pyim-page-tooltip 'posframe)
;;   ;; (setq pyim-page-tooltip 'popup)
;;   ;; set candicates size
;;   (setq pyim-page-length 5)
;;   (setq pyim-dicts
;;    (quote
;;     ((:name "bigdict" :file "~/.emacs.d/pyim/dicts/pyim-bigdict.pyim")))))

;;; 60-im.el ends here
