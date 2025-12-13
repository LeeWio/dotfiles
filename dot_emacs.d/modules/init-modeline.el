;;; init-modeline.el --- Modeline 配置

;; Doom Modeline
(use-package doom-modeline
  :ensure t
  :demand t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 28
        doom-modeline-bar-width 3
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-minor-modes nil
        doom-modeline-buffer-encoding nil
        doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-project-detection 'auto
        doom-modeline-vcs-max-length 20
        doom-modeline-buffer-state-icon t
        doom-modeline-buffer-modification-icon t
        doom-modeline-time-icon nil
        doom-modeline-enable-word-count nil
        doom-modeline-checker-simple-format t
        doom-modeline-vcs-icon t
        doom-modeline-lsp t
        doom-modeline-github nil
        doom-modeline-modal nil
        doom-modeline-mu4e nil
        doom-modeline-gnus nil
        doom-modeline-irc nil
        doom-modeline-env-version nil))

;; Modeline 透明背景
(with-eval-after-load 'doom-modeline
  (custom-set-faces
   '(mode-line ((t (:background unspecified :box nil))))
   '(mode-line-inactive ((t (:background unspecified :box nil))))))

(provide 'init-modeline)
;;; init-modeline.el ends here
