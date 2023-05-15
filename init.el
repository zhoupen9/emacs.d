;;; init.el --- Emacs init
;;; Commentary:

;;; Code:

(when (eq system-type 'darwin)
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
  (add-to-list 'load-path "~/.local/share/emacs/site-lisp"))

;; Initialize packages
(when (< emacs-major-version 27)
  (load (concat user-emacs-directory "early-init"))
  (package-initialize))

(require 'use-package)

(use-package package
  :custom
  (package-pinned-packages
   '((yasnippet . "elpa"))))

(defconst emacs-profile-dir (concat user-emacs-directory "profile.d/") "Emacs Profile directory.")

(global-unset-key "\C-z")

(setenv "XAPIAN_CJK_NGRAM" "1")

;; add site-themes to theme load path
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes"))

;; add profile directory to load path
;; (add-to-list 'load-path emacs-profile-dir)

(defun emacs-profile--load (elisp-file)
  "Load profile pacakge from ELISP-FILE."
  (load (file-name-sans-extension elisp-file)))

(mapc 'emacs-profile--load
      (directory-files emacs-profile-dir t ".el$"))

;;; init.el ends here

