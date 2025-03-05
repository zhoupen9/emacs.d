;;; -*- lexical-binding: t; -*-
;;; 20-ai.el --- Site start
;;; Commentary:

;;; Code:

(defvar home-directory)
(defvar emacs-data-dir)

(use-package gptel
  :custom
  (gptel-temperature 0.1)
  (gptel-model 'Qwen2.5-Coder-7B-Instruct-Q6)
  (gptel-backend (gptel-make-openai "llama-cpp"
                        :stream t
                        :header '(("Authorization" . "Bearer llama.cpp.s3cr3t"))
                        :protocol "http"
                        :host "localhost:22022"
                        :models '(Qwen2.5-Coder-7B-Instruct-Q6))))

(use-package aider
  ;;:straight (:host github :repo "tninja/aider.el" :files ("aider.el"))
  :config
  ;; Use claude-3-5-sonnet cause it is best in aider benchmark
  (setq aider-args '("--model" "openai/Qwen2.5-Coder-7B-Instruct-Q6"))
  (setenv "OPENAI_API_KEY" "llama.cpp.s3cr3t")
  (setenv "OPENAI_API_BASE" "http://127.0.0.1:22022")
  ;;(setenv "ANTHROPIC_API_KEY" anthropic-api-key)
  ;; Or use chatgpt model since it is most well known
  ;; (setq aider-args '("--model" "o3-mini"))
  ;; (setenv "OPENAI_API_KEY" <your-openai-api-key>)
  ;; Or use gemini v2 model since it is very good and free
  ;; (setq aider-args '("--model" "gemini/gemini-exp-1206"))
  ;; (setenv "GEMINI_API_KEY" <your-gemini-api-key>)
  ;; Or use your personal config file
  ;; (setq aider-args `("--config" ,(expand-file-name "~/.aider.conf.yml")))
  ;; ;;
  ;; Optional: Set a key binding for the transient menu
  (global-set-key (kbd "C-c a") 'aider-transient-menu))

;;; 20-ai.el ends herev
;;; End:
